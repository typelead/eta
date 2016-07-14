module GHCVM.CodeGen.Main where

import Module
import HscTypes
import Type
import TyCon
import StgSyn
import DynFlags
import FastString
import VarEnv
import Id
import Name
import OccName
import DataCon
import Util (unzipWith)

import GHCVM.Util
import GHCVM.Primitive
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Closure
import GHCVM.CodeGen.Con
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Bind
import GHCVM.CodeGen.Name
import GHCVM.CodeGen.Rts
import GHCVM.CodeGen.ArgRep
import GHCVM.CodeGen.Env

import Codec.JVM

import Data.Maybe (mapMaybe)
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Control.Monad (unless)

import Data.Text (Text, pack, cons, append)

codeGen :: HscEnv -> Module -> [TyCon] -> [StgBinding] -> HpcInfo -> IO [ClassFile]
codeGen hscEnv thisMod dataTyCons stgBinds _hpcInfo =
  runCodeGen env state $ do
      mapM_ (cgTopBinding dflags) stgBinds
      mapM_ cgTyCon dataTyCons
  where
    (env, state) = initCg dflags thisMod
    dflags = hsc_dflags hscEnv

cgTopBinding :: DynFlags -> StgBinding -> CodeGen ()
cgTopBinding dflags (StgNonRec id rhs) = do
  mod <- getModule
  id' <- externaliseId dflags id
  let (info, code) = cgTopRhs dflags NonRecursive id' rhs
  code
  addBinding info

cgTopBinding dflags (StgRec pairs) = do
  mod <- getModule
  let (binders, rhss) = unzip pairs
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
  where cgIdInfo = mkCgIdInfo id lfInfo
        lfInfo = mkClosureLFInfo dflags id TopLevel [] updateFlag args
        (modClass, clName, clClass) = getJavaInfo cgIdInfo
        isThunk = isLFThunk lfInfo
        qClName = closure clName
        genCode dflags _
          | StgApp f [] <- body, null args, isNonRec recflag
          = do cgInfo <- getCgIdInfo f
               let loadCode = idInfoLoadCode cgInfo
               defineField $ mkFieldDef [Public, Static, Final] qClName indStaticType
               addInitStep $ fold
                 [
                   new stgIndStatic,
                   dup indStaticType,
                   loadCode,
                   invokespecial $ mkMethodRef stgIndStatic "<init>"
                     [closureType] void,
                   putstatic $ mkFieldRef modClass qClName indStaticType
                 ]
        genCode dflags lf = do
          CgState { cgClassName } <- forkClosureBody $
            closureCodeBody True id lfInfo
                            (nonVoidIds args) (length args) body []

          let ft = obj cgClassName
          -- NOTE: Don't make thunks final so that they can be
          --       replaced by their values by the GC
          let flags = (if isThunk then [] else [Final]) ++ [Public, Static]
          defineField $
            mkFieldDef flags qClName ft
          addInitStep $ fold
            [
              new cgClassName,
              dup ft,
              invokespecial $ mkMethodRef cgClassName "<init>" [] void,
              putstatic $ mkFieldRef modClass qClName ft
            ]
          return ()

-- Simplifies the code if the mod is associated to the Id
externaliseId :: DynFlags -> Id -> CodeGen Id
externaliseId dflags id
  | isInternalName name = setIdName id . externalise <$> getModule
  | otherwise           = return id
  where
    externalise mod = mkExternalName uniq mod new_occ loc
    name    = idName id
    uniq    = nameUnique name
    new_occ = mkLocalOcc uniq $ nameOccName name
    loc     = nameSrcSpan name

cgTyCon :: TyCon -> CodeGen ()
cgTyCon tyCon = unless (null dataCons) $ do
    CgState {..} <- newTypeClosure tyConClass stgConstr
    mapM_ (cgDataCon cgClassName) (tyConDataCons tyCon)
  where tyConClass = nameTypeText . tyConName $ tyCon
        dataCons = tyConDataCons tyCon

cgDataCon :: Text -> DataCon -> CodeGen ()
cgDataCon typeClass dataCon = do
  modClass <- getModClass
  let dataConClassName = nameText . dataConName $ dataCon
      thisClass = qualifiedName modClass dataConClassName
      thisFt = obj thisClass
  if isNullaryRepDataCon dataCon then do
      newExportedClosure dataConClassName typeClass $
        defineMethod $ mkDefaultConstructor thisClass typeClass
      return ()
  else
    do let initCode :: Code
           initCode = fold . flip map indexedFields $ \(i, ft) ->
             let maybeDup = if i /= numFields then dup thisFt else mempty
             in maybeDup
             <> gload ft (fromIntegral i)
             <> putfield (mkFieldRef thisClass (varX i) ft)

           varX :: Int -> Text
           varX n = cons 'x' . pack . show $ n

           getterX :: Int -> Text
           getterX n = append "get" . pack . show $ n

           getterDefs :: [MethodDef]
           getterDefs =
             flip map indexedFields $ \(i, ft) ->
               mkMethodDef thisClass [Public] (getterX i) [] (ret ft) $ fold
               [
                 gload thisFt 0,
                 getfield $ mkFieldRef thisClass (varX i) ft,
                 greturn ft
               ]

           fieldDefs :: [FieldDef]
           fieldDefs = map (\(i, ft) ->
                         mkFieldDef [Private, Final] (varX i) ft)
                       indexedFields

           indexedFields :: [(Int, FieldType)]
           indexedFields = indexList fields

           numFields :: Int
           numFields = length fields

           fields :: [FieldType]
           fields = repFieldTypes $ dataConRepArgTys dataCon

       newExportedClosure dataConClassName typeClass $ do
         defineFields fieldDefs
         defineMethods getterDefs
         defineMethod $ mkConstructorDef thisClass typeClass fields initCode
       return ()
