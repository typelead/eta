module Eta.TypeCheck.TcMatches where
import Eta.HsSyn.HsSyn    ( GRHSs, MatchGroup, LHsExpr )
import Eta.TypeCheck.TcEvidence( HsWrapper )
import Eta.BasicTypes.Name     ( Name )
import Eta.TypeCheck.TcType   ( TcRhoType )
import Eta.TypeCheck.TcRnTypes( TcM, TcId )
--import Eta.BasicTypes.SrcLoc   ( Located )

tcGRHSsPat    :: GRHSs Name (LHsExpr Name)
              -> TcRhoType
              -> TcM (GRHSs TcId (LHsExpr TcId))

tcMatchesFun :: Name -> Bool
             -> MatchGroup Name (LHsExpr Name)
             -> TcRhoType
             -> TcM (HsWrapper, MatchGroup TcId (LHsExpr TcId))
