{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Boots.Factory.Consul(
    buildConsul
  ) where

import           Boots
import           Boots.Factory.Client ()
import           Boots.Web
import           Control.Monad        (void)
import qualified Data.HashMap.Strict  as HM
import           Data.Version         (showVersion)
import           Network.Consul

-- | Register consule service.
buildConsul
  :: (MonadMask n, MonadIO n, HasSalak env, HasApp env, HasLogger env)
  => Factory n (WebEnv env context) ()
buildConsul = tryBuildByKey False "consul.enabled" $ do
  AppEnv{..}          <- asksEnv (view askApp)
  WebConfig{..}       <- view askWebConfig <$> getEnv
  mst                 <- require "consul.client"
  cc@ConsulConfig{..} <- require "consul"
  let
    ConsulApi{..} = consulApi cc mst
    met2 = HM.insert "version" (fromString $ showVersion version) meta
    open = registerService
      $ newServer
      $ HttpServer name instanceId (Just hostname) (Just port) tags met2
      $ ServiceCheck name instanceId interval dcsa
      $ "http://" <> hostname <> ":" <> show port <>"/endpoints/health"
    close _ = void
      $ deregisterService instanceId
  delay $ logInfo "Service deregistered from consul."
  _ <- produce open close
  logInfo "Service registered to consul."



















