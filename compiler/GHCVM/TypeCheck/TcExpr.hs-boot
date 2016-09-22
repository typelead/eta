module GHCVM.TypeCheck.TcExpr where
import GHCVM.HsSyn.HsSyn    ( HsExpr, LHsExpr )
import GHCVM.BasicTypes.Name     ( Name )
import GHCVM.TypeCheck.TcType   ( TcType, TcRhoType, TcSigmaType )
import GHCVM.TypeCheck.TcRnTypes( TcM, TcId, CtOrigin )

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
