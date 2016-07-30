module GHCVM.DeSugar.DsExpr where
import HsSyn    ( HsExpr, LHsExpr, HsLocalBinds )
import Var      ( Id )
import GHCVM.DeSugar.DsMonad  ( DsM )
import CoreSyn  ( CoreExpr )

dsExpr  :: HsExpr  Id -> DsM CoreExpr
dsLExpr :: LHsExpr Id -> DsM CoreExpr
dsLocalBinds :: HsLocalBinds Id -> CoreExpr -> DsM CoreExpr