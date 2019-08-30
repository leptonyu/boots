{-# LANGUAGE ConstraintKinds #-}
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
  -- * Environment
    bootApp
  -- * Monad App
  , module Boots.App
  -- * Factory Instances
  , module Boots.Factory.Application
  , module Boots.Factory.Salak
  , module Boots.Factory.Logger
  -- * Components
  , module Boots.Health
  , module Boots.Random
  -- * CLI
  , module Boots.CLI
  -- * Reexport
  , module Control.Monad.Factory
  , module Boots.Prelude
  ) where

import           Boots.App
import           Boots.CLI
import           Boots.Factory.Application
import           Boots.Factory.Logger
import           Boots.Factory.Salak
import           Boots.Health
import           Boots.Prelude
import           Boots.Random
import           Control.Monad.Factory
import           Data.Version              (Version)

-- | An out-of-box application booter, with builtin components. Also supports a default commandline handling.
bootApp
  :: String -- ^ name
  -> Version -- ^ version
  -> Factory IO (AppEnv ()) env -- ^ Generate env
  -> Factory IO (AppEnv env) (IO ()) -- ^ Application body.
  -> IO ()
bootApp n ver fext fac = runCLI ver
  $ \cli -> boot
  $ buildApp n ver cli () >>> go >>> fac
  where
    {-# INLINE go #-}
    go = do
      ext1       <- fext
      AppEnv{..} <- getEnv
      return AppEnv{ext = ext1, ..}
