module Eta.DeSugar.DsExpr where
import Eta.HsSyn.HsSyn    ( HsExpr, LHsExpr, HsLocalBinds )
import Eta.BasicTypes.Var      ( Id )
import Eta.DeSugar.DsMonad  ( DsM )
import Eta.Core.CoreSyn  ( CoreExpr )

dsExpr  :: HsExpr  Id -> DsM CoreExpr
dsLExpr :: LHsExpr Id -> DsM CoreExpr
dsLocalBinds :: HsLocalBinds Id -> CoreExpr -> DsM CoreExpr