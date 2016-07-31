module GHCVM.Main.Packages where
-- Well, this is kind of stupid...
import {-# SOURCE #-} GHCVM.BasicTypes.Module (PackageKey)
import {-# SOURCE #-} GHCVM.Main.DynFlags (DynFlags)
data PackageState
packageKeyPackageIdString :: DynFlags -> PackageKey -> Maybe String