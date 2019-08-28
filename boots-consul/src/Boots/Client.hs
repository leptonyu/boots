{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Boots.Client where

import           Boots
import           Network.HTTP.Client hiding (Proxy)
import           Salak

instance Default ManagerSettings where
  def = defaultManagerSettings

instance FromProp m ResponseTimeout where
  fromProp = responseTimeoutMicro <$> fromProp

instance FromProp m ManagerSettings where
  fromProp = do
    connCount <- "max-conns"  .?: managerConnCount
    timeout   <- "timeout"    .?: managerResponseTimeout
    idleCount <- "idle-conns" .?: managerIdleConnectionCount
    return def
      { managerConnCount           = connCount
      , managerResponseTimeout     = timeout
      , managerIdleConnectionCount = idleCount
      }

managerLogException :: HasLogger env => env -> ManagerSettings -> ManagerSettings
managerLogException env ms  = ms { managerWrapException = go }
  where
    go req ma = do
      runAppT env $ logInfo $ "Request: " <> toLogStr (show req)
      managerWrapException ms req ma
