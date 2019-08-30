{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Boots.Endpoint.Info(
    endpointInfo
  ) where

import           Boots
import           Boots.Endpoint.Class
import           Boots.Factory.Web
import           Control.Concurrent
import           Data.Aeson
import           Data.Text            (Text)
import           Data.Version         (Version, showVersion)
import           GHC.Generics
import           GHC.RTS.Flags
import           Servant
import           System.Info

data Info = Info
  { name       :: !Text
  , instanceId :: !Text
  , version    :: !Version
  , profile    :: !Bool
  } deriving (Show, Generic, ToSchema)

type EndpointInfo = "info" :> Get '[JSON] Info

-- | Register info endpoint.
endpointInfo
  :: (HasWeb context env, MonadMask n, MonadIO n)
  => Proxy context
  -> Factory n (WebEnv env context) ()
endpointInfo pc = do
  WebEnv{..} <- getEnv
  registerEndpoint "info" pc (Proxy @EndpointInfo) $ liftIO $ do
    rtsf <- getRTSFlags
    return (go rtsf envs)
  where
    {-# INLINE go #-}
    go RTSFlags{..} AppEnv{..}=
      let ProfFlags{..} = profilingFlags
      in Info{profile = find doHeapProfile , ..}
    {-# INLINE find #-}
    find NoHeapProfiling = False
    find _               = True

instance ToJSON Info where
  toJSON Info{..} = object
    [ "application"   .= name
    , "instanceId"    .= instanceId
    , "version"       .= version
    , "isMultithread" .= rtsSupportsBoundThreads
    , "isProfile"     .= profile
    , "os"            .= os
    , "arch"          .= arch
    , "compiler"      .= (compilerName <> "-" <> showVersion compilerVersion)
    ]
