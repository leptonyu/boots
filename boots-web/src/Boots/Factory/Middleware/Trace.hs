{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Boots.Factory.Middleware.Trace where

import           Boots
import           Boots.Factory.Web
import           Network.HTTP.Types

{-# INLINE hTraceId #-}
hTraceId :: HeaderName
hTraceId = "X-B3-TraceId"

{-# INLINE hSpanId #-}
hSpanId :: HeaderName
hSpanId = "X-B3-SpanId"

buildTrace
  :: (HasLogger env, HasContextEntry context env, MonadMask n, MonadIO n)
  => Proxy context -> Proxy env ->Factory n (WebEnv env context) ()
buildTrace pc pe = registerVault pc pe "Trace" askLogger $ return . addTrace ("hello" :: String)
