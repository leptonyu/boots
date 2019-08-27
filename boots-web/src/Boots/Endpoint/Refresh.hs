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
  } deriving (Eq, Show, Generic, ToJSON, ToSchema)

endpointRefresh
  ::( MonadMask n
    , MonadIO n
    , HasSalak env
    , HasLogger env
    , HasContextEntry context env)
  => Proxy context
  -> Factory n (WebEnv env context) ()
endpointRefresh pc = do
  reload <- askReload
  makeEndpoint "refresh" pc (Proxy @EndpointRefresh) (liftIO $ go <$> reload)
  where
    {-# INLINE go #-}
    go ReloadResult{..} = Refresh{..}
