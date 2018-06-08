{-# LANGUAGE OverloadedStrings #-}
module Eta.CodeGen.Constr where


import Eta.Main.DynFlags
import Eta.BasicTypes.Id

import Eta.BasicTypes.DataCon
import Eta.StgSyn.StgSyn


import Eta.CodeGen.Types
import Eta.CodeGen.Monad
import Eta.CodeGen.Closure
import Eta.CodeGen.ArgRep
import Eta.CodeGen.Env
import Eta.CodeGen.Name
import Eta.CodeGen.Rts
import Eta.Utils.Util

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
  where cgIdInfo                      = mkCgIdInfo dflags id (Just dataFt) lfInfo
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
              singleton = null args
              mFieldCode
                | singleton = Nothing
                | otherwise = Just (field, fold loadCodes)
          when (not singleton) $
            defineField $ mkFieldDef [Private, Static, Volatile] qClName closureType
          return $ Just (modClass, qClName, dataClass, mFieldCode, recIndexes)

buildDynCon :: Id -> DataCon -> [StgArg] -> [Id] -> CodeGen ( CgIdInfo
                                                            , CodeGen (Code, RecIndexes, FieldType) )
buildDynCon binder con [] _ = do
  dflags <- getDynFlags
  let dataFt = obj (dataConClass dflags con)
  return ( mkCgIdInfo dflags binder (Just dataFt) (mkConLFInfo con)
         , return (mempty, [], dataFt) )
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
  return (idInfo, genCode dflags cgLoc)
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

        genCode dflags cgLoc = do
          (recIndexes, loadsCode) <- foldM foldLoads ([], mempty) indexFtArgs
          let conCode =
                  new dataFt
               <> dup dataFt
               <> loadsCode
               <> invokespecial (mkMethodRef dataClass "<init>" fields void)
          return (mkRhsInit cgLoc conCode, recIndexes, dataFt)
          where dataFt    = obj dataClass
                dataClass = dataConClass dflags con
