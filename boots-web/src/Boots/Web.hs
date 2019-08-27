{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Boots.Web(
    bootWeb
  , module Boots.Factory.Web
  --
  , module Boots.Metrics
  , module Boots.Middleware.Endpoint
  , module Boots.Middleware.Error
  , module Boots.Middleware.Logger
  , module Boots.Middleware.Random
  , module Boots.Middleware.Trace
  ) where

import           Boots.Factory.Web
import           Boots.Metrics

import           Boots.Middleware.Endpoint
import           Boots.Middleware.Error
import           Boots.Middleware.Logger
import           Boots.Middleware.Random
import           Boots.Middleware.Trace

import           Boots
import           Data.Version              (Version)
import           Servant


bootWeb
  :: forall api env context
  . ( HasServer api context
    , HasContextEntry context env
    , HasLogger env
    , HasSalak env
    , HasRandom env
    , HasApp env
    , HasHealth env)
  => String
  -> Version
  -> (AppEnv -> env)
  -> (env -> Context context)
  -> (Proxy context -> Proxy env -> Factory IO (WebEnv env context) ())
  -> Proxy api
  -> ServerT api (App env)
  -> IO ()
bootWeb appName ver fenv fcxt buildCustom api server = boot $ do
  app  <- buildApp appName ver
  within app $ do
    conf  <- require "application"
    store <- liftIO newStore
    logInfo $ "Start Service [" <> toLogStr (name app) <> "] ..."
    let
      c = newWebEnv
         (fenv app)
         fcxt
         conf
         store :: WebEnv env context
      pe = Proxy @env
      pc = Proxy @context
    within c $ do
      tryServe True  pc api server
      buildError     pc pe
      buildCustom    pc pe
      buildWebLogger pc pe
      buildTrace     pc pe
      buildRandom    pc pe
      buildEndpoints pc pe
      buildWeb       pc pe
