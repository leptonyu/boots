{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Boots.Endpoint.Refresh(
   endpointRefresh
  ) where

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

-- | Register refresh endpoint.
endpointRefresh
  :: (HasWeb context env, MonadMask n, MonadIO n)
  => Proxy context
  -> Factory n (WebEnv env context) ()
endpointRefresh pc = do
  reload <- askReload
  registerEndpoint "refresh" pc (Proxy @EndpointRefresh) (liftIO $ go <$> reload)
  where
    {-# INLINE go #-}
    go ReloadResult{..} = Refresh{..}
