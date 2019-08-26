{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
module Boots.Factory.Middleware.Logger(
    buildWebLogger
  , toMonadLogger
  , L.runLoggingT
  ) where

import           Boots
import           Boots.Factory.Web
import qualified Control.Monad.Logger as L
import           GHC.Stack
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
  env <- askEnv
  registerMiddleware $ \app req resH -> app req
    $ \res -> do
      runVault env (vault req) $ toLog req (responseStatus res)
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
