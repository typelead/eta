module Eta.BasicTypes.IdInfo where
import Eta.Utils.Outputable
data IdInfo
data IdDetails

vanillaIdInfo :: IdInfo
coVarDetails :: IdDetails
pprIdDetails :: IdDetails -> SDoc
