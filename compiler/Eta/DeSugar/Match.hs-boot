module Eta.DeSugar.Match where
import Eta.BasicTypes.Var      ( Id )
import Eta.TypeCheck.TcType   ( Type )
import Eta.DeSugar.DsMonad  ( DsM, EquationInfo, MatchResult )
import Eta.Core.CoreSyn  ( CoreExpr )
import Eta.HsSyn.HsSyn    ( LPat, HsMatchContext, MatchGroup, LHsExpr )
import Eta.BasicTypes.Name     ( Name )

match   :: [Id]
        -> Type
        -> [EquationInfo]
        -> DsM MatchResult

matchWrapper
        :: HsMatchContext Name
        -> Maybe (LHsExpr Id)
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
