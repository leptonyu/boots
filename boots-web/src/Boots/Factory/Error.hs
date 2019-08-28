{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module:      Boots.Factory.Error
-- Copyright:   2019 Daniel YU
-- License:     MIT
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module provide supports for handling exception.
--
module Boots.Factory.Error(
    buildError
  , buildWebLogger
  , toMonadLogger
  , L.runLoggingT
  ) where

import           Boots
import           Boots.Factory.Web
import           Control.Exception    (catch)
import qualified Control.Monad.Logger as L
import           GHC.Stack
import           Network.HTTP.Types
import           Network.Wai

-- | Catch exception, convert to Resonpose.
{-# INLINE buildError #-}
buildError
  :: forall context env n
  . (HasWeb context env, MonadMask n, MonadIO n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildError _ _ = tryBuildByKey True "web.error.enabled" $
  registerMiddleware $ \app env req resH -> app env req resH `catch`
    \e -> do
      runAppT env $ logException e
      resH (whenException e)

-- | Register logging requests.
{-# INLINE buildWebLogger #-}
buildWebLogger
  :: forall context env n
  . (HasWeb context env, MonadMask n, MonadIO n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildWebLogger _ _ = tryBuildByKey True "web.log.enabled" $
  registerMiddleware $ \app env req resH -> app env req
    $ \res -> do
      runAppT env $ toLog req (responseStatus res)
      resH res

{-# INLINE toLog #-}
toLog :: HasLogger env => Request -> Status -> App env ()
toLog ~req Status{..} =
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

-- | Adapter to [monad-logger](https://hackage.haskell.org/package/monad-logger).
{-# INLINE toMonadLogger #-}
toMonadLogger :: ToLogStr msg => LogFunc -> L.Loc -> L.LogSource -> L.LogLevel -> msg -> IO ()
toMonadLogger LogFunc{..} L.Loc{..} _ ll = logfunc g1 (g2 ll) . toLogStr
  where
    {-# INLINE g1 #-}
    g1 = uncurry (uncurry (SrcLoc loc_package loc_module loc_filename) loc_start) loc_end
    {-# INLINE g2 #-}
    g2 L.LevelDebug     = LevelDebug
    g2 L.LevelInfo      = LevelInfo
    g2 L.LevelWarn      = LevelWarn
    g2 L.LevelError     = LevelError
    g2 (L.LevelOther _) = LevelTrace
