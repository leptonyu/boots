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
-- * Motivation
--
-- Simplify to create an application in Haskell.
--
-- When we decide to create an application using Haskell.
-- We may need using configurations, loggers as basic functions.
-- If this application needs storages, caches, etc.,
-- then we have to weaving the management of connection of these facilities into the application.
-- Connections need to be created before and be destroyed after using them.
-- There is a common strategy to manage connections, that is using `Control.Monad.Cont`.
-- Then we can encapsulate the management of connections separately.
-- For example, we can write a database plugin `Plugin` @cxt@ @m@ @DBConnection@,
-- which can manage the database connections in monad @m@ with context @cxt@.
-- Context @cxt@ may be requested for getting configurations or logging functions.
-- When all the components of application are encapsulated by plugins, then building an application will be simplified.
--
-- * Have a Try
--
-- >>> bootApp (pluginSimple "application") (logInfo "hello")
-- 2019-07-27 19:35:30  INFO [application] Ghci1 - hello
-- >>> bootApp (pluginSimple "application") (require "user" >>= logInfo)
-- 2019-07-27 19:37:45  INFO [application] Ghci2 - daniel
--
-- Main
--
-- > main :: IO ()
-- > main = bootApp (pluginSimple "application") go
-- >   where
-- >     go = forever $ do
-- >       user <- require "user"              -- Request for configuration.
-- >       logInfo $ "Hello, " <> user <> "!"  -- Request for logging.
-- >       liftIO $ threadDelay 1000000
--

module Boots(
    module Boots.Internal
  , module Boots.Plugin.Simple
  ) where

import           Boots.Internal
import           Boots.Plugin.Simple





