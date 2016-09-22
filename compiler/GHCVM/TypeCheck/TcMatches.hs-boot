module GHCVM.TypeCheck.TcMatches where
import GHCVM.HsSyn.HsSyn    ( GRHSs, MatchGroup, LHsExpr )
import GHCVM.TypeCheck.TcEvidence( HsWrapper )
import GHCVM.BasicTypes.Name     ( Name )
import GHCVM.TypeCheck.TcType   ( TcRhoType )
import GHCVM.TypeCheck.TcRnTypes( TcM, TcId )
--import GHCVM.BasicTypes.SrcLoc   ( Located )

tcGRHSsPat    :: GRHSs Name (LHsExpr Name)
              -> TcRhoType
              -> TcM (GRHSs TcId (LHsExpr TcId))

tcMatchesFun :: Name -> Bool
             -> MatchGroup Name (LHsExpr Name)
             -> TcRhoType
             -> TcM (HsWrapper, MatchGroup TcId (LHsExpr TcId))
