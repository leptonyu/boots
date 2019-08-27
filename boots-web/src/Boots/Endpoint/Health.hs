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

endpointHealth
  ::( MonadMask n
    , MonadIO n
    , HasHealth env
    , HasLogger env
    , HasContextEntry context env)
  => Proxy context
  -> EndpointConfig
  -> Factory n (WebEnv env context) ()
endpointHealth pc conf = do
  health <- asksEnv (view askHealth)
  makeEndpoint conf "logger" pc (Proxy @EndpointHealth) $ liftIO $ do
    h@Health{..} <- health
    if status == UP then return h else throwM err400 { errBody = encode h }
