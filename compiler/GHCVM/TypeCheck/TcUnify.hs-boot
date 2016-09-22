module GHCVM.TypeCheck.TcUnify where
import GHCVM.TypeCheck.TcType     ( TcTauType )
import GHCVM.TypeCheck.TcRnTypes  ( TcM )
import GHCVM.TypeCheck.TcEvidence ( TcCoercion )

-- This boot file exists only to tie the knot between
--              TcUnify and Inst

unifyType :: TcTauType -> TcTauType -> TcM TcCoercion
