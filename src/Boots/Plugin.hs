-- |
-- Module:      Boots.Plugin
-- Copyright:   2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Boot plugins.
--
module Boots.Plugin(
  -- * Simple Plugin
    module Boots.Plugin.Simple
  -- * Configuration Plugin
  , module Boots.Plugin.Salak
  -- * Logger Plugin
  , module Boots.Plugin.Logger
  ) where

import           Boots.Plugin.Logger
import           Boots.Plugin.Salak
import           Boots.Plugin.Simple
