module Eta.TypeCheck.TcRnDriver where

import Eta.Types.Type          (TyThing)
import Eta.TypeCheck.TcRnTypes (TcM, TypeError(..))
import Eta.BasicTypes.Name     (Name)
import Eta.Main.DynFlags       (DynFlags)

checkBootDeclM :: Bool  -- ^ True <=> an hs-boot file (could also be a sig)
               -> TyThing -> TyThing -> TcM ()
missingBootThing :: Bool -> Name -> String -> TypeError
badReexportedBootThing :: DynFlags -> Bool -> Name -> Name -> TypeError
