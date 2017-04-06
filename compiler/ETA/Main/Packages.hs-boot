module ETA.Main.Packages where
import {-# SOURCE #-} ETA.Main.DynFlags(DynFlags)
import {-# SOURCE #-} ETA.BasicTypes.Module(ComponentId, UnitId, InstalledUnitId)
data PackageState
data PackageConfigMap
emptyPackageState :: PackageState
componentIdString :: DynFlags -> ComponentId -> Maybe String
displayInstalledUnitId :: DynFlags -> InstalledUnitId -> Maybe String
improveUnitId :: PackageConfigMap -> UnitId -> UnitId
getPackageConfigMap :: DynFlags -> PackageConfigMap