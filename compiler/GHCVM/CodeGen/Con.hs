module GHCVM.CodeGen.Con where

import Literal
import DynFlags hiding (mAX_INTLIKE, mIN_INTLIKE, mAX_INTLIKE, mAX_CHARLIKE)
import Id
import Module
import DataCon
import StgSyn
import PrelInfo (maybeCharLikeCon, maybeIntLikeCon)
import Outputable
import GHCVM.Constants
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Closure
import GHCVM.CodeGen.ArgRep
import GHCVM.CodeGen.Env
import GHCVM.CodeGen.Name
import GHCVM.CodeGen.Rts
import Data.Foldable (fold)
import Codec.JVM
import Data.Maybe (catMaybes)
import Data.Char (ord)

cgTopRhsCon :: DynFlags
            -> Id               -- Name of thing bound to this RHS
            -> DataCon          -- Id
            -> [StgArg]         -- Args
            -> (CgIdInfo, CodeGen ())
cgTopRhsCon dflags id dataCon args = (cgIdInfo, genCode)
  where cgIdInfo = mkCgIdInfo id lfInfo
        lfInfo = mkConLFInfo dataCon
        maybeFields = map repFieldType $ dataConRepArgTys dataCon
        fields = catMaybes maybeFields
        (modClass, clName, dataClass) = getJavaInfo cgIdInfo
        qClName = closure clName
        dataFt = obj dataClass
        typeFt = obj (tyConClass (dataConTyCon dataCon))
        genCode = do
          loads <- mapM getArgLoadCode . getNonVoids $ zip maybeFields args
          defineField $ mkFieldDef [Public, Static] qClName closureType
          addInitStep $ fold
            [
              new dataFt,
              dup dataFt,
              fold loads,
              invokespecial $ mkMethodRef dataClass "<init>" fields void,
              putstatic $ mkFieldRef modClass qClName closureType
            ]

buildDynCon :: Id -> DataCon -> [StgArg] -> CodeGen (CgIdInfo, CodeGen Code)
buildDynCon binder con [] = return
  ( mkCgIdInfo binder (mkConLFInfo con)
  , return mempty )
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
buildDynCon binder con args = do
  (idInfo, cgLoc) <- rhsIdInfo binder lfInfo
  let (_, _, dataClass) = getJavaInfo idInfo
  return (idInfo, genCode cgLoc dataClass)
  where lfInfo = mkConLFInfo con
        maybeFields = map repFieldType $ dataConRepArgTys con
        fields = catMaybes maybeFields
        genCode cgLoc dataClass = do
          loads <- mapM getArgLoadCode . getNonVoids $ zip maybeFields args
          let conCode = fold
                [
                  new dataFt,
                  dup dataFt,
                  fold loads,
                  invokespecial $ mkMethodRef dataClass "<init>" fields void
                ]
          return $ mkRhsInit cgLoc conCode
          where dataFt = obj dataClass
