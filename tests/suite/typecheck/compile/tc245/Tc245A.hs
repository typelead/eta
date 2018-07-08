
{-# LANGUAGE TypeFamilies #-}
module Tc245A where
class Foo a where
    data Bar a :: * -> *
