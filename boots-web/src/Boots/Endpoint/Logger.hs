{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module Boots.Endpoint.Logger where

import           Boots
import           Boots.Endpoint.Class
import           Boots.Factory.Web
import           Data.Aeson
import           GHC.Generics
import           Salak
import           Servant

type EndpointLogger = "logger" :> (Get '[JSON] LogInfo :<|> ReqBody '[JSON] LogInfo :> Put '[JSON] NoContent)

newtype LogInfo = LogInfo
  { level :: String
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Register logger endpoint.
endpointLogger
  :: (HasWeb context env, MonadMask n, MonadIO n)
  => Proxy context
  -> Factory n (WebEnv env context) ()
endpointLogger pc = do
  LogFunc{..} <- asksEnv (view askLogger)
  registerEndpoint "logger" pc (Proxy @EndpointLogger) (getLogInfo logLvl :<|> putLogInfo logLvl)
  where
    getLogInfo w = liftIO $ LogInfo . show <$> getWritable w
    putLogInfo w LogInfo{..} = liftIO $ setWritable (rightToMaybe $ levelFromStr $ fromString level) w >> return NoContent
