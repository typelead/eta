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
import Data.Foldable (fold)
import Codec.JVM
import Data.Maybe(catMaybes)

cgTopRhsCon :: DynFlags
            -> Module
            -> Id               -- Name of thing bound to this RHS
            -> DataCon          -- Id
            -> [StgArg]         -- Args
            -> (CgIdInfo, CodeGen ())
cgTopRhsCon dflags mod id dataCon args = (cgIdInfo, genCode)
  where cgIdInfo@CgIdInfo {..} = mkCgIdInfo mod id lambdaFormInfo
        lambdaFormInfo = mkConLFInfo dataCon
        maybeFields = repFieldTypeMaybes $ dataConRepArgTys dataCon
        fields = catMaybes maybeFields
        dataFt = cgFieldType
        dataClass = cgClosureClass
        genCode = do
          loads <- mapM loadArgCode .  getNonVoids $ zip maybeFields args
          defineField $ mkFieldDef [Public, Static, Final] cgClosureName dataFt
          addInitStep $ fold
            [
              new dataClass,
              dup dataFt,
              fold loads,
              invokespecial $ mkMethodRef dataClass "<init>" fields void,
              putstatic $ mkFieldRef cgModuleClass cgClosureName dataFt
            ]
