module Eta.TypeCheck.TcUnify where
import Eta.TypeCheck.TcType     ( TcTauType )
import Eta.TypeCheck.TcRnTypes  ( TcM )
import Eta.TypeCheck.TcEvidence ( TcCoercion )

-- This boot file exists only to tie the knot between
--              TcUnify and Inst

unifyType :: TcTauType -> TcTauType -> TcM TcCoercion
