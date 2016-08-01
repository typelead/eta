module GHCVM.BasicTypes.MkId where
import GHCVM.BasicTypes.Name( Name )
import GHCVM.BasicTypes.Var( Id )
import {-# SOURCE #-} GHCVM.BasicTypes.DataCon( DataCon )
import {-# SOURCE #-} GHCVM.Prelude.PrimOp( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkPrimOpId      :: PrimOp -> Id

magicDictId :: Id
