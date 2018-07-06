module Eta.Main.Packages where
import {-# SOURCE #-} Eta.Main.DynFlags(DynFlags)
import {-# SOURCE #-} Eta.BasicTypes.Module(ComponentId, UnitId, InstalledUnitId)

data PackageState
data PackageConfigMap

emptyPackageState      :: PackageState
componentIdString      :: DynFlags -> ComponentId -> Maybe String
displayInstalledUnitId :: DynFlags -> InstalledUnitId -> Maybe String
improveUnitId          :: PackageConfigMap -> UnitId -> UnitId
getPackageConfigMap    :: DynFlags -> PackageConfigMap
