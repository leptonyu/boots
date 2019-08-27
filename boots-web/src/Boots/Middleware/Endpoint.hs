{-# LANGUAGE OverloadedStrings #-}
module Boots.Middleware.Endpoint where

import           Boots
import           Boots.Endpoint.Class
import           Boots.Endpoint.Health
import           Boots.Endpoint.Info
import           Boots.Endpoint.Logger
import           Boots.Endpoint.Metrics
import           Boots.Endpoint.Refresh
import           Boots.Factory.Web

buildEndpoints
  :: forall context env n
  . ( HasContextEntry context env
    , HasSalak env
    , HasApp env
    , HasHealth env
    , HasLogger env
    , MonadIO n
    , MonadMask n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildEndpoints pc _ = do
  ec@EndpointConfig{..} <- require "web.endpoint"
  tryBuild enabled $ do
    endpointInfo    pc ec
    endpointLogger  pc ec
    endpointRefresh pc ec
    endpointHealth  pc ec
    endpointMetrics pc ec
