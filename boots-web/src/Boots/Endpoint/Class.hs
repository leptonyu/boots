{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Boots.Endpoint.Class(
    registerEndpoint
  ) where

import           Boots
import           Boots.Factory.Web
import qualified Data.HashMap.Strict     as HM
import qualified Data.Swagger            as S
import           Data.Text               (Text)
import           Servant
import           Servant.Server.Internal

data EndpointTag

instance HasServer api ctx
  => HasServer (EndpointTag :> api) ctx where
  type ServerT (EndpointTag :> api) m = ServerT api m
  route _ b = pathRouter "endpoints" . route (Proxy @api) b
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance HasSwagger api => HasSwagger (EndpointTag :> api) where
  toSwagger _ = toSwagger (Proxy @api) & S.applyTags [S.Tag "endpoints" (Just "Endpoints API") Nothing]

-- | Register endpoint, use this function to create custom endpoints.
registerEndpoint
  :: forall context env api n
  . ( HasSwagger api
    , HasServer api context
    , HasWeb context env
    , MonadIO n
    , MonadMask n)
  => Text -- ^ Endpoint name, used for path, @/endpoints/:name@.
  -> Proxy context -- ^ Context proxy.
  -> Proxy api -- ^ Api proxy.
  -> ServerT api (App (AppEnv env)) -- ^ Api server.
  -> Factory n (WebEnv env context) ()
registerEndpoint name pc _ server = do
  WebEnv{..} <- getEnv
  let ok = enabled endpoint && HM.lookup name (endpoints endpoint) /= Just False
  when ok $ logDebug $ "Endpoint " <> toLogStr name <> " actived."
  tryServeWithSwagger ok pc (Proxy @(EndpointTag :> api)) server
