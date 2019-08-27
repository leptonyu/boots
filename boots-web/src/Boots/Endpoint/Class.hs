{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Boots.Endpoint.Class where

import           Boots
import           Boots.Factory.Web
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe
import           Data.Text               (Text)
import           Salak
import           Servant
import           Servant.Server.Internal

data EndpointTag

instance HasServer api ctx
  => HasServer (EndpointTag :> api) ctx where
  type ServerT (EndpointTag :> api) m = ServerT api m
  route _ b = pathRouter "endpoints" . (route (Proxy @api) b)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

data EndpointConfig = EndpointConfig
  { enabled   :: Bool
  , endpoints :: HM.HashMap Text Bool
  }

instance FromProp m EndpointConfig where
  fromProp = EndpointConfig
    <$> "enabled" .?= True
    <*> "enabled" .?= HM.empty

makeEndpoint
  :: forall context env api n
  . ( MonadMask n
    , MonadIO n
    , HasLogger env
    , HasServer api context
    , HasContextEntry context env)
  => EndpointConfig
  -> Text
  -> Proxy context
  -> Proxy api
  -> ServerT api (App env)
  -> Factory n (WebEnv env context) ()
makeEndpoint EndpointConfig{..} name pc _ server = do
  let ok = fromMaybe True $ HM.lookup name endpoints
  when ok $ logInfo $ "Endpoint " <> toLogStr name <> " actived."
  tryServe ok pc (Proxy @(EndpointTag :> api)) server
