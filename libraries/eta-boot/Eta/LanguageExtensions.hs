{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module re-exports the 'Extension' type along with an orphan 'Binary'
-- instance for it.
--
-- Note that the @eta-boot@ package has a large set of dependencies; for this
-- reason the 'Extension' type itself is defined in the
-- "Eta.LanguageExtensions.Type" module provided by the @eta-boot-meta@ package,
-- which has no dependencies outside of @base@. For this reason
-- @eta-meta@ depends upon @eta-boot-meta@, not @eta-boot@.
--
module Eta.LanguageExtensions ( module Eta.LanguageExtensions.Type ) where

import Data.Binary
import Eta.LanguageExtensions.Type

instance Binary Extension
