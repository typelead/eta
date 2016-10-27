module ETA.DeSugar.Match where
import ETA.BasicTypes.Var      ( Id )
import ETA.TypeCheck.TcType   ( Type )
import ETA.DeSugar.DsMonad  ( DsM, EquationInfo, MatchResult )
import ETA.Core.CoreSyn  ( CoreExpr )
import ETA.HsSyn.HsSyn    ( LPat, HsMatchContext, MatchGroup, LHsExpr )
import ETA.BasicTypes.Name     ( Name )

match   :: [Id]
        -> Type
        -> [EquationInfo]
        -> DsM MatchResult

matchWrapper
        :: HsMatchContext Name
        -> MatchGroup Id (LHsExpr Id)
        -> DsM ([Id], CoreExpr)

matchSimply
        :: CoreExpr
        -> HsMatchContext Name
        -> LPat Id
        -> CoreExpr
        -> CoreExpr
        -> DsM CoreExpr

matchSinglePat
        :: CoreExpr
        -> HsMatchContext Name
        -> LPat Id
        -> Type
        -> MatchResult
        -> DsM MatchResult