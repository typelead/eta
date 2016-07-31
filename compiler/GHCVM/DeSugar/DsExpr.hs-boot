module GHCVM.DeSugar.DsExpr where
import GHCVM.HsSyn.HsSyn    ( HsExpr, LHsExpr, HsLocalBinds )
import GHCVM.BasicTypes.Var      ( Id )
import GHCVM.DeSugar.DsMonad  ( DsM )
import GHCVM.Core.CoreSyn  ( CoreExpr )

dsExpr  :: HsExpr  Id -> DsM CoreExpr
dsLExpr :: LHsExpr Id -> DsM CoreExpr
dsLocalBinds :: HsLocalBinds Id -> CoreExpr -> DsM CoreExpr