module GHCVM.BasicTypes.IdInfo where
import GHCVM.Utils.Outputable
data IdInfo
data IdDetails

vanillaIdInfo :: IdInfo
coVarDetails :: IdDetails
pprIdDetails :: IdDetails -> SDoc
