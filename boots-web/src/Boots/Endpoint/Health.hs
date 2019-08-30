{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Boots.Endpoint.Health(
    endpointHealth
  ) where

import           Boots
import           Boots.Endpoint.Class
import           Boots.Factory.Web
import           Data.Aeson
import           Servant


type EndpointHealth = "health" :> Get '[JSON] Health

instance ToJSON HealthStatus
instance ToJSON Health where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance ToSchema HealthStatus
instance ToSchema Health

-- | Register health endpoint.
endpointHealth
  :: (HasWeb context env, MonadMask n, MonadIO n)
  => Proxy context
  -> Factory n (WebEnv env context) ()
endpointHealth pc = do
  health <- asksEnv (view askHealth)
  registerEndpoint "health" pc (Proxy @EndpointHealth) $ liftIO $ do
    h@Health{..} <- health
    if status == UP then return h else throwM err400 { errBody = encode h }
