{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Boots.Endpoint.Info where

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
  { name    :: !Text
  , version :: !Version
  , profile :: !Bool
  } deriving (Show, Generic)

type EndpointInfo = "info" :> Get '[JSON] Info


endpointInfo
  ::( MonadMask n
    , MonadIO n
    , HasLogger env
    , HasApp env
    , HasContextEntry context env)
  => Proxy context
  -> EndpointConfig
  -> Factory n (WebEnv env context) ()
endpointInfo pc conf = do
  app <- asksEnv (view askApp)
  makeEndpoint conf "info" pc (Proxy @EndpointInfo) $ liftIO $ do
    rtsf <- getRTSFlags
    return (go rtsf app)
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
    , "version"       .= version
    , "isMultithread" .= rtsSupportsBoundThreads
    , "isProfile"     .= profile
    , "os"            .= os
    , "arch"          .= arch
    , "compiler"      .= (compilerName <> "-" <> showVersion compilerVersion)
    ]
