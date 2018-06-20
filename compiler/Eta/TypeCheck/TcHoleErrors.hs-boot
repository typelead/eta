-- This boot file is in place to break the loop where:
-- + TcSimplify calls 'TcErrors.reportUnsolved',
-- + which calls 'TcHoleErrors.findValidHoleFits`
-- + which calls 'TcSimplify.simpl_top'
module Eta.TypeCheck.TcHoleErrors where

import Eta.TypeCheck.TcRnTypes  ( TcM, Ct, Implication )
import Eta.Utils.Outputable ( SDoc )
import Eta.BasicTypes.VarEnv     ( TidyEnv )

findValidHoleFits :: TidyEnv -> [Implication] -> [Ct] -> Ct
                  -> TcM (TidyEnv, SDoc)
