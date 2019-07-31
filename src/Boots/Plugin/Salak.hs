-- |
-- Module:      Boots.Plugin.Salak
-- Copyright:   2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module wrap 'salak' into a plugin.
--
module Boots.Plugin.Salak(
    HasSalak(..)
  , pluginSalak
  -- ** Configuration Functions
  , MonadSalak(..)
  ) where

import           Boots.Internal
import           Control.Monad.Reader
import           Lens.Micro
import           Lens.Micro.Extras
import           Salak
import           Salak.Yaml

-- | Environment providing a configuration parser.
class HasSalak env where
  askSourcePack :: Lens' env SourcePack

instance HasSalak SourcePack where
  askSourcePack = id

instance HasSalak env => MonadSalak (Plugin env m) where
  askSalak = asks (view askSourcePack)

instance (Monad m, HasSalak env) => MonadSalak (AppT env m) where
  askSalak = asks (view askSourcePack)

-- | Plugin used for parse properties.
pluginSalak
  :: (MonadIO m, MonadCatch m)
  => String -- ^ Configuration file name.
  -> Plugin () m SourcePack
pluginSalak name = lift $ runSalakWithYaml name askSalak
