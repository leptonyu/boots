{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Boots.Endpoint.Refresh where

import           Boots
import           Boots.Endpoint.Class
import           Boots.Factory.Web
import           Data.Aeson
import           GHC.Generics
import           Salak
import           Servant

type EndpointRefresh = "refresh" :> Post '[JSON] Refresh

data Refresh = Refresh
  { hasError :: !Bool
  , msgs     :: ![String]
  } deriving (Eq, Show, Generic, ToJSON)

endpointRefresh
  ::( MonadMask n
    , MonadIO n
    , HasLogger env
    , HasSalak env
    , HasContextEntry context env)
  => Proxy context
  -> EndpointConfig
  -> Factory n (WebEnv env context) ()
endpointRefresh pc conf = do
  reload <- askReload
  makeEndpoint conf "logger" pc (Proxy @EndpointRefresh) (liftIO $ go <$> reload)
  where
    {-# INLINE go #-}
    go ReloadResult{..} = Refresh{..}
