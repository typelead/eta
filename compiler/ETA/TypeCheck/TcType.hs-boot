module ETA.TypeCheck.TcType where
import ETA.Utils.Outputable( SDoc )

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc