module ETA.TypeCheck.TcMatches where
import ETA.HsSyn.HsSyn    ( GRHSs, MatchGroup, LHsExpr )
import ETA.TypeCheck.TcEvidence( HsWrapper )
import ETA.BasicTypes.Name     ( Name )
import ETA.TypeCheck.TcType   ( TcRhoType )
import ETA.TypeCheck.TcRnTypes( TcM, TcId )
--import ETA.BasicTypes.SrcLoc   ( Located )

tcGRHSsPat    :: GRHSs Name (LHsExpr Name)
              -> TcRhoType
              -> TcM (GRHSs TcId (LHsExpr TcId))

tcMatchesFun :: Name -> Bool
             -> MatchGroup Name (LHsExpr Name)
             -> TcRhoType
             -> TcM (HsWrapper, MatchGroup TcId (LHsExpr TcId))
