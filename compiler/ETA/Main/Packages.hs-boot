module ETA.Main.Packages where
-- Well, this is kind of stupid...
import {-# SOURCE #-} ETA.BasicTypes.Module (PackageKey)
import {-# SOURCE #-} ETA.Main.DynFlags (DynFlags)
data PackageState
packageKeyPackageIdString :: DynFlags -> PackageKey -> Maybe String