{-# LANGUAGE OverloadedStrings #-}
module ETA.CodeGen.Main where

import ETA.BasicTypes.Module
import ETA.Main.HscTypes
import ETA.Types.Type
import ETA.Types.TyCon
import ETA.StgSyn.StgSyn
import ETA.Main.DynFlags
import ETA.Utils.FastString
import ETA.BasicTypes.VarEnv
import ETA.BasicTypes.Id
import ETA.BasicTypes.Name
import ETA.BasicTypes.OccName
import ETA.BasicTypes.DataCon
import ETA.Utils.Util (unzipWith)
import ETA.Prelude.PrelNames (rOOT_MAIN)

import ETA.Util

import ETA.Debug
import ETA.CodeGen.Types
import ETA.CodeGen.Closure
import ETA.CodeGen.Constr
import ETA.CodeGen.Monad
import ETA.CodeGen.Bind
import ETA.CodeGen.Name
import ETA.CodeGen.Rts
import ETA.CodeGen.ArgRep
import ETA.CodeGen.Env

import Codec.JVM

import Data.Maybe (mapMaybe, catMaybes)
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Control.Monad (unless, when)

import Data.Text (Text, pack, cons, append)

codeGen :: HscEnv -> Module -> [TyCon] -> [StgBinding] -> HpcInfo -> IO [ClassFile]
codeGen hscEnv thisMod dataTyCons stgBinds _hpcInfo = do
  runCodeGen env state $ do
      mapM_ (cgTopBinding dflags) stgBinds
      mapM_ cgTyCon dataTyCons
  where
    (env, state) = initCg dflags thisMod
    dflags = hsc_dflags hscEnv

cgTopBinding :: DynFlags -> StgBinding -> CodeGen ()
cgTopBinding dflags (StgNonRec id rhs) = do
  debugDoc $ str "generating " <+> ppr id
  mod <- getModule
  id' <- externaliseId dflags id
  let (info, code) = cgTopRhs dflags NonRecursive id' rhs
  code
  addBinding info

cgTopBinding dflags (StgRec pairs) = do
  mod <- getModule
  let (binders, rhss) = unzip pairs
  debugDoc $ str "generating (rec) " <+> ppr binders
  binders' <- mapM (externaliseId dflags) binders
  let pairs'         = zip binders' rhss
      r              = unzipWith (cgTopRhs dflags Recursive) pairs'
      (infos, codes) = unzip r
  addBindings infos
  sequence_ codes

cgTopRhs :: DynFlags -> RecFlag -> Id -> StgRhs -> (CgIdInfo, CodeGen ())
cgTopRhs dflags _ binder (StgRhsCon _ con args) =
  cgTopRhsCon dflags binder con args

cgTopRhs dflags recflag binder
   (StgRhsClosure _ binderInfo freeVars updateFlag _ args body) =
  -- fvs should be empty
  cgTopRhsClosure dflags recflag binder binderInfo updateFlag args body

cgTopRhsClosure :: DynFlags
                -> RecFlag              -- member of a recursive group?
                -> Id
                -> StgBinderInfo
                -> UpdateFlag
                -> [Id]                 -- Args
                -> StgExpr
                -> (CgIdInfo, CodeGen ())
cgTopRhsClosure dflags recflag id binderInfo updateFlag args body
  = (cgIdInfo, genCode dflags lfInfo)
  where cgIdInfo = mkCgIdInfo dflags id lfInfo
        lfInfo = mkClosureLFInfo id TopLevel [] updateFlag args
        (modClass, clName, clClass) = getJavaInfo dflags cgIdInfo
        isThunk = isLFThunk lfInfo
        qClName = closure clName
        genCode dflags _
          | StgApp f [] <- body, null args, isNonRec recflag
          = do cgInfo <- getCgIdInfo f
               let loadCode = idInfoLoadCode cgInfo
               defineField $ mkFieldDef [Public, Static] qClName closureType
               let field = mkFieldRef modClass qClName closureType
                   deps = catMaybes [getLocField (cgLocation cgInfo)]
               addInitStep (fold
                 [
                   new indStaticType,
                   dup indStaticType,
                   loadCode,
                   invokespecial $ mkMethodRef stgIndStatic "<init>"
                     [closureType] void,
                   putstatic field
                 ]
                 , field
                 , deps
                 -- TODO: Check this works when f is in external module
                 )
        genCode dflags lf = do
          (_, CgState { cgClassName }) <- forkClosureBody $
            closureCodeBody True id lfInfo
                            (nonVoidIds args) (length args) body [] False []

          let ft = obj cgClassName
          -- NOTE: Don't make thunks final so that they can be
          --       replaced by their values by the GC
          let flags = (if isThunk then [] else [Final]) ++ [Public, Static]
          defineField $ mkFieldDef flags qClName closureType
          let field = mkFieldRef modClass qClName closureType
          addInitStep (fold
            [
              new ft,
              dup ft,
              invokespecial $ mkMethodRef cgClassName "<init>" [] void,
              putstatic field
            ]
            , field
            , []
            )
          return ()

-- Simplifies the code if the mod is associated to the Id
externaliseId :: DynFlags -> Id -> CodeGen Id
externaliseId dflags id = do
  mod <- getModule
  return $
    if isInternalName name then
      setIdName id $ externalise mod
    else if isExternalName name && nameModule name == rOOT_MAIN then
      setIdName id $ internalise mod
    else id
  where
    internalise mod = mkExternalName uniq mod occ' loc
      where occ' = mkOccName ns $ ":" ++ occNameString occ
    externalise mod = mkExternalName uniq mod occ' loc
      where occ' = mkLocalOcc uniq occ
    name = idName id
    uniq = nameUnique name
    occ  = nameOccName name
    loc  = nameSrcSpan name
    ns   = occNameSpace occ

cgTyCon :: TyCon -> CodeGen ()
cgTyCon tyCon = unless (null dataCons) $ do
    dflags <- getDynFlags
    let tyConClass = nameTypeText dflags . tyConName $ tyCon
    (_, CgState {..}) <- newTypeClosure tyConClass stgConstr
    mapM_ (cgDataCon cgClassName) (tyConDataCons tyCon)
    when (isEnumerationTyCon tyCon) $
      cgEnumerationTyCon cgClassName tyCon
  where dataCons = tyConDataCons tyCon

cgEnumerationTyCon :: Text -> TyCon -> CodeGen ()
cgEnumerationTyCon tyConCl tyCon = do
  dflags <- getDynFlags
  thisClass <- getClass
  let fieldName = nameTypeTable dflags $ tyConName tyCon
      loadCodes = [    dup arrayFt
                    <> iconst jint i
                    <> new dataFt
                    <> dup dataFt
                    <> invokespecial (mkMethodRef dataClass "<init>" [] void)
                    <> gastore elemFt
                    | (i, con) <- zip [0..] $ tyConDataCons tyCon
                    , let dataFt    = obj dataClass
                          dataClass = dataConClass dflags con ]
  defineField $ mkFieldDef [Public, Static, Final] fieldName arrayFt
  let field = mkFieldRef thisClass fieldName arrayFt
  addInitStep (fold
    [
      iconst jint $ fromIntegral familySize,
      new arrayFt,
      fold loadCodes,
      putstatic field
    ]
    , field
    , []
    )
  where
        arrayFt = jarray elemFt
        elemFt = obj tyConCl
        familySize = tyConFamilySize tyCon

cgDataCon :: Text -> DataCon -> CodeGen ()
cgDataCon typeClass dataCon = do
  dflags <- getDynFlags
  modClass <- getModClass
  let dataConClassName = nameDataText dflags . dataConName $ dataCon
      thisClass = qualifiedName modClass dataConClassName
      thisFt = obj thisClass
      defineTagMethod =
          defineMethod . mkMethodDef thisClass [Public] "getTag" [] (ret jint) $
                         iconst jint conTag
                      <> greturn jint
  -- TODO: Reduce duplication
  if isNullaryRepDataCon dataCon then do
      newExportedClosure dataConClassName typeClass $ do
        defineMethod $ mkDefaultConstructor thisClass typeClass
        defineTagMethod
      return ()
  else
    do let initCode :: Code
           initCode = go 1 indexedFields
             where go _ [] = mempty
                   go n ((i, ft): xs) = code <> go (n + fieldSize ft) xs
                    where maybeDup = if i /= numFields then dup thisFt else mempty
                          code     = maybeDup
                                  <> gload ft (fromIntegral n)
                                  <> putfield (mkFieldRef thisClass (constrField i) ft)

           fieldDefs :: [FieldDef]
           fieldDefs = map (\(i, ft) ->
                        -- TODO: Find a better way to handle recursion
                        --       that allows us to use 'final' in most cases.
                         mkFieldDef [Public] (constrField i) ft)
                       indexedFields


           (ps, os, ns, fs, ls, ds) = go indexedFields [] [] [] [] [] []

           go [] ps os ns fs ls ds = (ps, os, ns, fs, ls, ds)
           go ((i, ft):ifs) ps os ns fs ls ds =
             case ftArgRep ft of
               P -> go ifs ((i, code):ps) os ns fs ls ds
               O -> go ifs ps ((i, code):os) ns fs ls ds
               N -> go ifs ps os ((i, code):ns) fs ls ds
               F -> go ifs ps os ns ((i, code):fs) ls ds
               L -> go ifs ps os ns fs ((i, code):ls) ds
               D -> go ifs ps os ns fs ls ((i, code):ds)
               _ -> panic "cgDataCon: V argrep!"
              where code = gload thisFt 0
                        <> getfield (mkFieldRef thisClass (constrField i) ft)

           defineGetRep :: ArgRep -> [(Int, Code)] -> CodeGen ()
           defineGetRep rep [] = return ()
           defineGetRep rep branches =
             defineMethod $
               mkMethodDef thisClass [Public] method [jint] (ret ft) $
                 gswitch (gload jint 1) branches
                   (Just $ barf (append method ": invalid field index!")
                        <> defaultValue ft)
              <> greturn ft
             where ft = argRepFt rep
                   method = append "get" (pack $ show rep)

           indexedFields :: [(Int, FieldType)]
           indexedFields = indexList fields

           numFields :: Int
           numFields = length fields

           fields :: [FieldType]
           fields = repFieldTypes $ dataConRepArgTys dataCon

       newExportedClosure dataConClassName typeClass $ do
         defineFields fieldDefs
         defineTagMethod
         defineGetRep P ps
         defineGetRep O os
         defineGetRep N ns
         defineGetRep F fs
         defineGetRep L ls
         defineGetRep D ds
         defineMethod $ mkConstructorDef thisClass typeClass fields initCode
       return ()
  where conTag = fromIntegral $ getDataConTag dataCon
