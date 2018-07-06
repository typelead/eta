module Eta.TypeCheck.TcRnDriver where

import Eta.Types.Type          (TyThing)
import Eta.TypeCheck.TcRnTypes (TcM)
import Eta.Utils.Outputable    (SDoc)
import Eta.BasicTypes.Name     (Name)
import Eta.Main.DynFlags       (DynFlags)

checkBootDeclM :: Bool  -- ^ True <=> an hs-boot file (could also be a sig)
               -> TyThing -> TyThing -> TcM ()
missingBootThing :: Bool -> Name -> String -> SDoc
badReexportedBootThing :: DynFlags -> Bool -> Name -> Name -> SDoc
