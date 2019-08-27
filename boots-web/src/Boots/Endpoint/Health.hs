{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Boots.Endpoint.Health where

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

endpointHealth
  ::( MonadMask n
    , MonadIO n
    , HasHealth env
    , HasLogger env
    , HasContextEntry context env)
  => Proxy context
  -> Factory n (WebEnv env context) ()
endpointHealth pc = do
  health <- asksEnv (view askHealth)
  makeEndpoint "health" pc (Proxy @EndpointHealth) $ liftIO $ do
    h@Health{..} <- health
    if status == UP then return h else throwM err400 { errBody = encode h }
