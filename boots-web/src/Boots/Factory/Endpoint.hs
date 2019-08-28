{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Boots.Factory.Endpoint
-- Copyright:   2019 Daniel YU
-- License:     MIT
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module provide supports for endpoints.
--
module Boots.Factory.Endpoint(
    buildEndpoints
  , registerEndpoint
  , endpointInfo
  , endpointLogger
  , endpointRefresh
  , endpointHealth
  , endpointMetrics
  ) where

import           Boots
import           Boots.Endpoint.Class
import           Boots.Endpoint.Health
import           Boots.Endpoint.Info
import           Boots.Endpoint.Logger
import           Boots.Endpoint.Metrics
import           Boots.Endpoint.Refresh
import           Boots.Factory.Web

-- | Register all endpoints provided by this package.
buildEndpoints
  :: forall context env n
  . (HasWeb context env, MonadMask n, MonadIO n)
  => Proxy context -- ^ Context proxy.
  -> Proxy env -- ^ Environment proxy.
  -> Factory n (WebEnv env context) ()
buildEndpoints pc _ = do
  WebEnv{..} <- getEnv
  unless   (enabled endpoint) $ logInfo "Endpoint is disabled."
  tryBuild (enabled endpoint) $ do
    endpointInfo    pc
    endpointLogger  pc
    endpointRefresh pc
    endpointHealth  pc
    endpointMetrics pc
