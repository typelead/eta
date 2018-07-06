module Eta.Main.PackageConfig where
import Eta.BasicTypes.FastString
import {-# SOURCE #-} Eta.BasicTypes.Module
import Eta.PackageDb

newtype PackageName     = PackageName FastString
newtype SourcePackageId = SourcePackageId FastString
type PackageConfig      = InstalledPackageInfo ComponentId SourcePackageId
                            PackageName UnitId ModuleName Module
