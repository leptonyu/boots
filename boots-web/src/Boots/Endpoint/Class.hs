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
import qualified Data.Swagger            as S
import           Data.Text               (Text)
import           Servant
import           Servant.Server.Internal

data EndpointTag

instance HasServer api ctx
  => HasServer (EndpointTag :> api) ctx where
  type ServerT (EndpointTag :> api) m = ServerT api m
  route _ b = pathRouter "endpoints" . (route (Proxy @api) b)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance HasSwagger api => HasSwagger (EndpointTag :> api) where
  toSwagger _ = toSwagger (Proxy @api) & S.applyTags [S.Tag "endpoints" (Just "Endpoints API") Nothing]

makeEndpoint
  :: forall context env api n
  . ( MonadMask n
    , MonadIO n
    , HasLogger env
    , HasSwagger api
    , HasServer api context
    , HasContextEntry context env)
  => Text
  -> Proxy context
  -> Proxy api
  -> ServerT api (App env)
  -> Factory n (WebEnv env context) ()
makeEndpoint name pc _ server = do
  WebEnv{..} <- getEnv
  let ok = fromMaybe True $ HM.lookup name $ endpoints endpoint
  when ok $ logDebug $ "Endpoint " <> toLogStr name <> " actived."
  tryServeWithSwagger ok pc (Proxy @(EndpointTag :> api)) server
