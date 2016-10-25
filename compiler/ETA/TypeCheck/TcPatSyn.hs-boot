module ETA.TypeCheck.TcPatSyn where

import ETA.BasicTypes.Name      ( Name )
import ETA.BasicTypes.Id        ( Id )
import ETA.HsSyn.HsSyn     ( PatSynBind, LHsBinds )
import ETA.TypeCheck.TcRnTypes ( TcM )
import ETA.BasicTypes.PatSyn    ( PatSyn )
import ETA.TypeCheck.TcPat     ( TcPatSynInfo )

tcInferPatSynDecl :: PatSynBind Name Name
                  -> TcM (PatSyn, LHsBinds Id)

tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (PatSyn, LHsBinds Id)

tcPatSynBuilderBind :: PatSynBind Name Name
                    -> TcM (LHsBinds Id)
