module ETA.BasicTypes.IdInfo where
import ETA.Utils.Outputable
data IdInfo
data IdDetails

vanillaIdInfo :: IdInfo
coVarDetails :: IdDetails
pprIdDetails :: IdDetails -> SDoc
