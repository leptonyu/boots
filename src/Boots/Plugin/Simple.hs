-- |
-- Module:      Boots.Plugin.Simple
-- Copyright:   2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module defines simple application plugin.
--
module Boots.Plugin.Simple(
    Simple(..)
  , HasSimple(..)
  , pluginSimple
  ) where

import           Boots.Internal
import           Boots.Plugin.Logger
import           Boots.Plugin.Salak
import           Data.Text           (Text, unpack)
import           Lens.Micro
import           Salak

-- | Simple plugin initialized both configurations 'pluginSalak' and logger 'pluginLogger'.
data Simple = Simple
  { sourcePack :: SourcePack
  , logFunc    :: LogFunc
  }

-- | Environment values with a configuration parser and logging function.
class HasSimple cxt where
  askSimple :: Lens' cxt Simple

instance HasSimple Simple where
  askSimple = id

instance HasLogger Simple where
  askLogger = lens logFunc (\x y -> x { logFunc = y})

instance HasSalak Simple where
  askSourcePack = lens sourcePack (\x y -> x {sourcePack = y})

-- | Simple plugin provides a configuration parser and logging function.
pluginSimple
  :: (MonadCatch m, MonadIO m)
  => Text -- ^ Application name and configuration file name.
  -> Plugin () m Simple
pluginSimple application = do
  sourcePack <- pluginSalak $ unpack application
  logFunc    <- promote sourcePack $ pluginLogger application
  return Simple{..}
