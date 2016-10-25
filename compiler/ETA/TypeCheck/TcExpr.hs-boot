module ETA.TypeCheck.TcExpr where
import ETA.HsSyn.HsSyn    ( HsExpr, LHsExpr )
import ETA.BasicTypes.Name     ( Name )
import ETA.TypeCheck.TcType   ( TcType, TcRhoType, TcSigmaType )
import ETA.TypeCheck.TcRnTypes( TcM, TcId, CtOrigin )

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
