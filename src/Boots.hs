-- |
-- Module:      Boots
-- Copyright:   2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Boot application by using plugins.
--
-- >>> booting (pluginSimple "application") (logInfo "hello")
-- 2019-07-27 19:35:30  INFO [application] Ghci1 - hello
-- >>> booting (pluginSimple "application") (require "user" >>= logInfo)
-- 2019-07-27 19:37:45  INFO [application] Ghci2 - daniel
--
module Boots(
    module Boots.Internal
  , module Boots.Plugin
  ) where

import           Boots.Internal
import           Boots.Plugin
