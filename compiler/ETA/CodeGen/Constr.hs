{-# LANGUAGE OverloadedStrings #-}
module ETA.CodeGen.Constr where


import ETA.Main.DynFlags
import ETA.BasicTypes.Id

import ETA.BasicTypes.DataCon
import ETA.StgSyn.StgSyn


import ETA.CodeGen.Types
import ETA.CodeGen.Monad
import ETA.CodeGen.Closure
import ETA.CodeGen.ArgRep
import ETA.CodeGen.Env
import ETA.CodeGen.Name
import ETA.CodeGen.Rts
import ETA.Util

import Control.Monad hiding (void)
import Codec.JVM
import Data.Maybe

import Data.Monoid ((<>))
import Data.Foldable (fold)

cgTopRhsCon :: DynFlags
            -> Id               -- Name of thing bound to this RHS
            -> [Id]             -- Recursive ids
            -> DataCon          -- Id
            -> [StgArg]         -- Args
            -> (CgIdInfo, CodeGen (Maybe RecInfo))
cgTopRhsCon dflags id conRecIds dataCon args = (cgIdInfo, genCode)
  where cgIdInfo                      = mkCgIdInfo dflags id lfInfo
        lfInfo                        = mkConLFInfo dataCon
        maybeFields                   = map repFieldType_maybe
                                      $ dataConRepArgTys dataCon
        fields                        = catMaybes maybeFields
        (modClass, clName, dataClass) = getJavaInfo dflags cgIdInfo
        qClName                       = closure clName
        dataFt                        = obj dataClass
        genCode                       = do
          let nvArgs = getNonVoids $ zip maybeFields args
          recCodes <- forM (zip [1..] nvArgs) $ \(i, nvArg) ->
                        case stgArgId (unsafeStripNV nvArg) of
                          Just nvId | nvId `elem` conRecIds
                            -> return (Just (i, nvId), aconst_null closureType)
                          _ -> fmap (Nothing,) $ getArgLoadCode nvArg
          let (mRecIndexes, loads) = unzip recCodes
              recIndexes           = catMaybes mRecIndexes
              field                = mkFieldRef modClass qClName closureType
              loadCodes            = [ new dataFt
                                     , dup dataFt
                                     , fold loads
                                     , invokespecial $
                                         mkMethodRef dataClass "<init>" fields void ]
          defineField $ mkFieldDef [Private, Static] qClName closureType
          return $ Just (modClass, qClName, dataClass, field, fold loadCodes, recIndexes)

buildDynCon :: Id -> DataCon -> [StgArg] -> [Id] -> CodeGen ( CgIdInfo
                                                            , CodeGen (Code, RecIndexes) )
buildDynCon binder con [] _ = do
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
  _ <- getDynFlags
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
        foldLoads (is, code) (i, (_, arg))
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
          return (mkRhsInit cgLoc conCode, recIndexes)
          where dataFt = locFt cgLoc
                dataClass = getFtClass dataFt
