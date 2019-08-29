{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
-- |
-- Module:      Boots.Web
-- Copyright:   2019 Daniel YU
-- License:     MIT
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- A quick out-of-box factory using to build web application with many useful builtin web components,
-- based on [boots-app](https://hackage.haskell.org/package/boots-app) and [servant](https://hackage.haskell.org/package/servant).
--
-- 1. Builtin metrics, use [ekg-core](https://hackage.haskell.org/package/ekg-core) as backend.
-- 2. Builtin tracing log, support B3-Tags.
-- 3. Builtin endpoints, info, healthcheck, logger, metrics, refresh configuration, swagger api.
-- 4. Builtin error management.
--
-- Hackage [boots-consul](https://hackage.haskell.org/package/boots-consul) provides consul support for building microservices.
--
module Boots.Web(
  -- * Boot web
    bootWeb
  , bootWebEnv
  , runContext
  , module Boots.Factory.Web
  -- * Metrics
  , module Boots.Metrics
  -- * Middleware
  -- ** Endpoint
  , module Boots.Factory.Endpoint
  -- ** Tracing
  , module Boots.Factory.Trace
  -- ** Other
  , module Boots.Factory.Error
  , module Boots.Factory.Random
  ) where

import           Boots.Factory.Web
import           Boots.Metrics

import           Boots.Factory.Endpoint
import           Boots.Factory.Error
import           Boots.Factory.Random
import           Boots.Factory.Trace

import           Boots
import           Data.Version           (Version)
import           Servant.API
import           Servant.Server

-- | Run context.
runContext :: HasContextEntry context env => Context context -> AppT env m () -> m ()
runContext = runAppT . getContextEntry

-- | A out-of-box web application booter with many predefined components.
bootWeb
  :: forall api env context
  . ( HasServer api context
    , HasSwagger api
    , HasWeb context env)
  => String -- ^ Application name.
  -> Version -- ^ Application version.
  -> Factory IO AppEnv env -- ^ Function which generates @env@ using `AppEnv`.
  -> Factory IO AppEnv (env -> Context context) -- ^ Function which generates @context@ using @env@.
  -> (Proxy context -> Proxy env -> Factory IO (WebEnv env context) ()) -- ^ Customized `Factory`.
  -> Proxy api -- ^ Api proxy.
  -> ServerT api (App env) -- ^ Servant api server.
  -> IO ()
bootWeb appName ver fenv fcxt buildCustom api server = boot $ do
  app  <- buildApp appName ver
  within app $ do
    conf  <- require "application"
    ec    <- require "endpoints"
    store <- liftIO newStore
    env   <- fenv
    cxt   <- fcxt
    logInfo $ "Start Service [" <> toLogStr (name app) <> "] ..."
    let
      c = newWebEnv env cxt conf ec store :: WebEnv env context
      pe = Proxy @env
      pc = Proxy @context
    within c $ do
      tryServeWithSwagger True  pc api server
      buildError     pc pe
      buildCustom    pc pe
      buildWebLogger pc pe
      buildTrace     pc pe
      buildRandom    pc pe
      buildEndpoints pc pe
      buildWeb       pc pe

-- | A out-of-box web application booter with many predefined components. A more generic version use `bootWeb`
bootWebEnv
  :: String
  -> Version
  -> Factory IO AppEnv ext
  -> Factory IO (WebEnv (Env ext) '[Env ext]) ()
  -> IO ()
bootWebEnv name ver makeExt mid
  = bootWeb name ver go (return (:. EmptyContext)) (\_ _ -> mid) (Proxy @EmptyAPI) emptyServer
  where
    go = do
      app <- getEnv
      ext <- makeExt
      return Env{..}


