-- |
-- Module:      Boots
-- Copyright:   2019 Daniel YU
-- License:     MIT
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- A quick out-of-box factory using to build application with many useful builtin components,
-- based on [boots](https://hackage.haskell.org/package/boots).
--
-- 1. Builtin configuration, use [salak](https://hackage.haskell.org/package/salak).
-- 2. Builtin logger functions, use [fast-logger](https://hackage.haskell.org/package/fast-logger) as backend.
-- 3. Builtin health check, support health check registration.
-- 4. Builtin random support, use [splitmix](https://hackage.haskell.org/package/splitmix) as backend.
-- 5. Define standard application values, such that @name@, @version@.
--
module Boots(
  -- * Monad App
    module Boots.App
  -- * Factory Instances
  , module Boots.Factory.Application
  , module Boots.Factory.Salak
  , module Boots.Factory.Logger
  -- * Components
  , module Boots.Health
  , module Boots.Random
  -- * Reexport
  , module Control.Monad.Factory
  , module Boots.Prelude
  ) where

import           Boots.App
import           Boots.Factory.Application
import           Boots.Factory.Logger
import           Boots.Factory.Salak
import           Boots.Health
import           Boots.Prelude
import           Boots.Random
import           Control.Monad.Factory



