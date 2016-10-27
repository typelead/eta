module ETA.TypeCheck.TcUnify where
import ETA.TypeCheck.TcType     ( TcTauType )
import ETA.TypeCheck.TcRnTypes  ( TcM )
import ETA.TypeCheck.TcEvidence ( TcCoercion )

-- This boot file exists only to tie the knot between
--              TcUnify and Inst

unifyType :: TcTauType -> TcTauType -> TcM TcCoercion
