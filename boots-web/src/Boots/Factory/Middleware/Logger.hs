{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
module Boots.Factory.Middleware.Logger where

import           Boots
import           Boots.Factory.Web
import           Network.HTTP.Types
import           Network.Wai

{-# INLINE buildWebLogger #-}
buildWebLogger
  :: forall context env n
  . ( HasLogger env
    , HasContextEntry context env
    , MonadIO n
    , MonadMask n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildWebLogger _ _ = do
  registerMiddleware "WebLog" $ \webNT app req resH -> app req $ \res -> do
    unNT webNT (vault req) (toLog req (responseStatus res))
    resH res

{-# INLINE toLog #-}
toLog :: HasLogger env => Request -> Status -> App env ()
toLog req Status{..} =
  let {-# INLINE g #-}
      g (Just i) = " \"" <> toLogStr i <> "\""
      g _        = " \"\""
      lf = if statusCode < 400 then logInfo else logWarn
  in lf $ "\""
    <> toLogStr (requestMethod req)
    <> " "
    <> toLogStr (rawPathInfo req)
    <> toLogStr (rawQueryString req)
    <> " "
    <> toLogStr (show $ httpVersion req)
    <> "\""
    <> g (requestHeaderReferer req)
    <> g (requestHeaderHost req)
    <> g (requestHeaderUserAgent req)
    <> " "
    <> toLogStr statusCode
