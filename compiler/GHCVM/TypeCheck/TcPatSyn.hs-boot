module GHCVM.TypeCheck.TcPatSyn where

import GHCVM.BasicTypes.Name      ( Name )
import GHCVM.BasicTypes.Id        ( Id )
import GHCVM.HsSyn.HsSyn     ( PatSynBind, LHsBinds )
import GHCVM.TypeCheck.TcRnTypes ( TcM )
import GHCVM.BasicTypes.PatSyn    ( PatSyn )
import GHCVM.TypeCheck.TcPat     ( TcPatSynInfo )

tcInferPatSynDecl :: PatSynBind Name Name
                  -> TcM (PatSyn, LHsBinds Id)

tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (PatSyn, LHsBinds Id)

tcPatSynBuilderBind :: PatSynBind Name Name
                    -> TcM (LHsBinds Id)
