module GHCVM.DeSugar.Match where
import GHCVM.BasicTypes.Var      ( Id )
import GHCVM.TypeCheck.TcType   ( Type )
import GHCVM.DeSugar.DsMonad  ( DsM, EquationInfo, MatchResult )
import GHCVM.Core.CoreSyn  ( CoreExpr )
import GHCVM.HsSyn.HsSyn    ( LPat, HsMatchContext, MatchGroup, LHsExpr )
import GHCVM.BasicTypes.Name     ( Name )

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