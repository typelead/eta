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

cgTopRhsCon :: DynFlags
            -> Module
            -> Id               -- Name of thing bound to this RHS
            -> DataCon          -- Id
            -> [StgArg]         -- Args
            -> (CgIdInfo, CodeGen ())
cgTopRhsCon dflags mod id dataCon args = (cgIdInfo, genCode)
  where cgIdInfo = mkCgIdInfo id lambdaFormInfo $
                     genClosureLoadCode mod (idName id) lambdaFormInfo
        lambdaFormInfo = mkConLFInfo dataCon
        genCode = do
         -- pprPanic ("cgTopRhsCon: " ++ (show . length $ args)) $ (ppr id) <> (ppr args) <> (ppr dataCon)
          -- initialize and create new object
          return ()
