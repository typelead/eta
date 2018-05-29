module Eta.BasicTypes.MkId where
import Eta.BasicTypes.Name( Name )
import Eta.BasicTypes.Var( Id )
import {-# SOURCE #-} Eta.BasicTypes.DataCon( DataCon )
import {-# SOURCE #-} Eta.Prelude.PrimOp( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkPrimOpId      :: PrimOp -> Id

magicDictId :: Id
