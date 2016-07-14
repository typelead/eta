module GHCVM.CodeGen.Con where

import DynFlags
import Id
import Module
import DataCon
import StgSyn
import Outputable
import GHCVM.CodeGen.Types
import GHCVM.CodeGen.Monad
import GHCVM.CodeGen.Closure
import GHCVM.CodeGen.ArgRep
import GHCVM.CodeGen.Env
import GHCVM.CodeGen.Name
import Data.Foldable (fold)
import Codec.JVM
import Data.Maybe(catMaybes)

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
        genCode = do
          loads <- mapM loadArgCode .  getNonVoids $ zip maybeFields args
          defineField $ mkFieldDef [Public, Static, Final] qClName dataFt
          addInitStep $ fold
            [
              new dataClass,
              dup dataFt,
              fold loads,
              invokespecial $ mkMethodRef dataClass "<init>" fields void,
              putstatic $ mkFieldRef modClass qClName dataFt
            ]
