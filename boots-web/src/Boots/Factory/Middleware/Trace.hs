{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
module Boots.Factory.Middleware.Trace where

import           Boots
import           Boots.Factory.Web
import           Data.ByteString    (ByteString)
import           Network.HTTP.Types
import           Network.Wai

{-# INLINE hTraceId #-}
hTraceId :: HeaderName
hTraceId = "X-B3-TraceId"

{-# INLINE hSpanId #-}
hSpanId :: HeaderName
hSpanId = "X-B3-SpanId"

buildTrace
  :: forall env context n
  . ( HasLogger env
    , HasRandom env
    , HasSalak env
    , HasContextEntry context env
    , MonadMask n
    , MonadIO n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildTrace _ _ = tryBuildByKey True "web.trace.enabled" $
  registerMiddleware $ \app env req resH -> do
    let x64 = runAppT env $ hex64 <$> nextW64 :: IO ByteString
    ids <- case lookup hTraceId (requestHeaders req) of
      Just tid -> (lookup hSpanId (requestHeaders req),tid,) <$> x64
      _        -> (Nothing,,) <$> x64 <*> x64
    app (over askLogger (addTrace $ go ids) env) req resH
  where
    {-# INLINE go #-}
    go :: (Maybe ByteString, ByteString, ByteString) -> LogStr
    go (Just pid, tid, sid) = toLogStr tid <> "," <> toLogStr sid <> "," <> toLogStr pid
    go (_, tid, sid) = toLogStr tid <> "," <> toLogStr sid <> ","
