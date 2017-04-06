module ETA.Main.PackageConfig where
import ETA.BasicTypes.FastString
import {-# SOURCE #-} ETA.BasicTypes.Module
import Eta.PackageDb
newtype PackageName = PackageName FastString
newtype SourcePackageId = SourcePackageId FastString
type PackageConfig = InstalledPackageInfo ComponentId SourcePackageId PackageName UnitId ModuleName Module