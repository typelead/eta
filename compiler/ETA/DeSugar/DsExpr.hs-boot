module ETA.DeSugar.DsExpr where
import ETA.HsSyn.HsSyn    ( HsExpr, LHsExpr, HsLocalBinds )
import ETA.BasicTypes.Var      ( Id )
import ETA.DeSugar.DsMonad  ( DsM )
import ETA.Core.CoreSyn  ( CoreExpr )

dsExpr  :: HsExpr  Id -> DsM CoreExpr
dsLExpr :: LHsExpr Id -> DsM CoreExpr
dsLocalBinds :: HsLocalBinds Id -> CoreExpr -> DsM CoreExpr