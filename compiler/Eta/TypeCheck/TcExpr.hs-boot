module Eta.TypeCheck.TcExpr where
import Eta.HsSyn.HsSyn    ( HsExpr, LHsExpr )
import Eta.BasicTypes.Name     ( Name )
import Eta.TypeCheck.TcType   ( TcType, TcRhoType, TcSigmaType )
import Eta.TypeCheck.TcRnTypes( TcM, TcId, CtOrigin )

tcPolyExpr ::
          LHsExpr Name
       -> TcSigmaType
       -> TcM (LHsExpr TcId)

tcMonoExpr, tcMonoExprNC ::
          LHsExpr Name
       -> TcRhoType
       -> TcM (LHsExpr TcId)

tcInferRho, tcInferRhoNC ::
          LHsExpr Name
       -> TcM (LHsExpr TcId, TcRhoType)

tcSyntaxOp :: CtOrigin
           -> HsExpr Name
           -> TcType
           -> TcM (HsExpr TcId)

tcCheckId :: Name -> TcRhoType -> TcM (HsExpr TcId)
