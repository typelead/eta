module ETA.BasicTypes.MkId where
import ETA.BasicTypes.Name( Name )
import ETA.BasicTypes.Var( Id )
import {-# SOURCE #-} ETA.BasicTypes.DataCon( DataCon )
import {-# SOURCE #-} ETA.Prelude.PrimOp( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkPrimOpId      :: PrimOp -> Id

magicDictId :: Id
