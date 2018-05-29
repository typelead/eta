module Eta.TypeCheck.TcPatSyn where

import Eta.BasicTypes.Name      ( Name )
import Eta.BasicTypes.Id        ( Id )
import Eta.HsSyn.HsSyn     ( PatSynBind, LHsBinds )
import Eta.TypeCheck.TcRnTypes ( TcM )
import Eta.BasicTypes.PatSyn    ( PatSyn )
import Eta.TypeCheck.TcPat     ( TcPatSynInfo )

tcInferPatSynDecl :: PatSynBind Name Name
                  -> TcM (PatSyn, LHsBinds Id)

tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (PatSyn, LHsBinds Id)

tcPatSynBuilderBind :: PatSynBind Name Name
                    -> TcM (LHsBinds Id)
