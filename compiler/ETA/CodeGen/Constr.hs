{-# LANGUAGE OverloadedStrings #-}
module ETA.CodeGen.Constr where

import ETA.BasicTypes.Literal
import ETA.Main.DynFlags hiding (mAX_INTLIKE, mIN_INTLIKE, mAX_INTLIKE, mAX_CHARLIKE)
import ETA.BasicTypes.Id
import ETA.BasicTypes.Module
import ETA.BasicTypes.DataCon
import ETA.StgSyn.StgSyn
import ETA.Prelude.PrelInfo (maybeCharLikeCon, maybeIntLikeCon)
import ETA.Main.Constants
import ETA.CodeGen.Types
import ETA.CodeGen.Monad
import ETA.CodeGen.Closure
import ETA.CodeGen.ArgRep
import ETA.CodeGen.Env
import ETA.CodeGen.Name
import ETA.CodeGen.Rts
import ETA.Util

import Control.Monad(foldM)
import Codec.JVM
import Data.Maybe (catMaybes, mapMaybe)
import Data.Char (ord)
import Data.Monoid ((<>))
import Data.Foldable (fold)

cgTopRhsCon :: DynFlags
            -> Id               -- Name of thing bound to this RHS
            -> DataCon          -- Id
            -> [StgArg]         -- Args
            -> (CgIdInfo, CodeGen ())
cgTopRhsCon dflags id dataCon args = (cgIdInfo, genCode)
  where cgIdInfo = mkCgIdInfo dflags id lfInfo
        lfInfo = mkConLFInfo dataCon
        maybeFields = map repFieldType_maybe $ dataConRepArgTys dataCon
        fields = catMaybes maybeFields
        (modClass, clName, dataClass) = getJavaInfo dflags cgIdInfo
        qClName = closure clName
        dataFt = obj dataClass
        typeFt = obj (tyConClass dflags (dataConTyCon dataCon))
        genCode = do
          loads <- mapM getArgLoadCode . getNonVoids $ zip maybeFields args
          deps <- getArgReferences . getNonVoids $ zip maybeFields args
          defineField $ mkFieldDef [Public, Static] qClName closureType
          let field = (mkFieldRef modClass qClName closureType)
          addInitStep (
              new dataFt
           <> dup dataFt
           <> fold loads
           <> invokespecial (mkMethodRef dataClass "<init>" fields void)
           <> putstatic field
           , field
           , deps
           )

buildDynCon :: Id -> DataCon -> [StgArg] -> [Id] -> CodeGen ( CgIdInfo
                                                            , CodeGen (Code, RecIndexes) )
buildDynCon binder con [] recIds = do
  dflags <- getDynFlags
  return ( mkCgIdInfo dflags binder (mkConLFInfo con)
         , return (mempty, []) )
-- buildDynCon binder con [arg]
--   | maybeIntLikeCon con
--   , StgLitArg (MachInt val) <- arg
--   , val <= fromIntegral mAX_INTLIKE
--   , val >= fromIntegral mIN_INTLIKE
--   = do
--       -- TODO: Generate offset into intlike array
--       unimplemented "buildDynCon: INTLIKE"
-- buildDynCon binder con [arg]
--   | maybeCharLikeCon con
--   , StgLitArg (MachChar val') <- arg
--   , let val = ord val' :: Int
--   , val <= fromIntegral mAX_INTLIKE
--   , val >= fromIntegral mIN_INTLIKE
--   = do
--       -- TODO: Generate offset into charlike array
--       unimplemented "buildDynCon: CHARLIKE"
buildDynCon binder con args recIds = do
  dflags <- getDynFlags
  (idInfo, cgLoc) <- rhsConIdInfo binder lfInfo
  return (idInfo, genCode cgLoc)
  where lfInfo = mkConLFInfo con
        maybeFields = map repFieldType_maybe $ dataConRepArgTys con
        nvFtArgs = mapMaybe (\(mft, arg) ->
                               case mft of
                                 Just ft -> Just (ft, arg)
                                 Nothing -> Nothing)
                   $ zip maybeFields args
        indexFtArgs = indexList nvFtArgs
        fields = catMaybes maybeFields
        -- TODO: Generalize to accommodate StdThunks as well
        foldLoads (is, code) (i, (ft, arg))
          | StgVarArg id <- arg
          , id `elem` recIds
          = return ((i, id):is, code <> aconst_null closureType)
          | otherwise = do
              loadCode <- getArgLoadCode (NonVoid arg)
              return (is, code <> loadCode)

        genCode cgLoc = do
          (recIndexes, loadsCode) <- foldM foldLoads ([], mempty) indexFtArgs
          let conCode =
                  new dataFt
               <> dup dataFt
               <> loadsCode
               <> invokespecial (mkMethodRef dataClass "<init>" fields void)
               -- <> fold (map (\i ->
               --                   dup dataFt
               --                <> dup dataFt
               --                <> putfield (mkFieldRef dataClass (constrField i) closureType))
               --           is)
          return (mkRhsInit cgLoc conCode, recIndexes)
          where dataFt = locFt cgLoc
                dataClass = getFtClass dataFt
