module Eta.Prelude.KnownUniques where


import Eta.BasicTypes.Unique
import Eta.BasicTypes.Name
import Eta.BasicTypes.BasicTypes

-- Needed by TysWiredIn
knownUniqueName :: Unique -> Maybe Name

mkSumTyConUnique :: Arity -> Unique
mkSumDataConUnique :: ConTagZ -> Arity -> Unique

mkCTupleTyConUnique :: Arity -> Unique
mkCTupleDataConUnique :: Arity -> Unique

mkTupleTyConUnique :: TupleSort -> Arity -> Unique
mkTupleDataConUnique :: TupleSort -> Arity -> Unique
