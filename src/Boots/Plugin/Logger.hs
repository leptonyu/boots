-- |
-- Module:      Boots.Plugin.Logger
-- Copyright:   2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module wrap a logging function into a plugin.
--
module Boots.Plugin.Logger(
    HasLogger(..)
  , LogConfig(..)
  , LogFunc
  , addTrace
  , pluginLogger
  -- ** Logger functions
  , logInfo
  , logDebug
  , logWarn
  , logError
  , logOther
  ) where

import           Boots.Internal
import           Boots.Plugin.Salak
import           Control.Monad
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Data.Default
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text, toLower, unpack)
import           Data.Word
import           Lens.Micro
import           Lens.Micro.Extras
import           Salak
import           System.Log.FastLogger

-- | Environment providing a logging function.
class HasLogger cxt where
  askLogger :: Lens' cxt LogFunc

instance HasLogger LogFunc where
  askLogger = id

instance (MonadIO m, HasLogger cxt) => MonadLogger (Plugin cxt m) where
  monadLoggerLog a b c d = do
    LogFunc{..} <- asks (view askLogger)
    liftIO $ logfunc a b c (toLogStr d)

instance (MonadIO m, HasLogger cxt) => MonadLogger (AppT cxt m) where
  monadLoggerLog a b c d = do
    LogFunc{..} <- asks (view askLogger)
    liftIO $ logfunc a b c (toLogStr d)

instance (MonadIO m, HasLogger cxt) => MonadLoggerIO (Plugin cxt m) where
  askLoggerIO = logfunc <$> asks (view askLogger)

instance (MonadIO m, HasLogger cxt) => MonadLoggerIO (AppT cxt m) where
  askLoggerIO = logfunc <$> asks (view askLogger)

instance Monad m => FromProp m LogLevel where
  fromProp = readEnum (fromEnumProp.toLower)
    where
      fromEnumProp "debug" = Right   LevelDebug
      fromEnumProp "info"  = Right   LevelInfo
      fromEnumProp "warn"  = Right   LevelWarn
      fromEnumProp "error" = Right   LevelError
      fromEnumProp u       = Left $ "unknown level: " ++ unpack u

{-# INLINE toStr #-}
toStr :: LogLevel -> LogStr
toStr LevelDebug     = "DEBUG"
toStr LevelInfo      = " INFO"
toStr LevelWarn      = " WARN"
toStr LevelError     = "ERROR"
toStr (LevelOther l) = toLogStr l

-- | Logger config.
data LogConfig = LogConfig
  { bufferSize    :: Word16         -- ^ Logger buffer size.
  , file          :: Maybe FilePath -- ^ Logger file path.
  , maxSize       :: Word32         -- ^ Max logger file size.
  , rotateHistory :: Word16         -- ^ Max number of logger files should be reserved.
  , level         :: IO LogLevel    -- ^ Log level to show.
  }
instance Default LogConfig where
  def = LogConfig 4096 Nothing 10485760 256 (return LevelInfo)

instance MonadIO m => FromProp m LogConfig where
  fromProp = LogConfig
    <$> "buffer-size" .?: bufferSize
    <*> "file"        .?: file
    <*> "max-size"    .?: maxSize
    <*> "max-history" .?: rotateHistory
    <*> "level"       .?: level

data LogFunc = LogFunc
  { logfunc :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  , logend  :: IO ()
  }

newLogger :: Text -> LogConfig -> IO LogFunc
newLogger name LogConfig{..} = do
  tc            <- newTimeCache "%Y-%m-%d %T"
  let ln = " [" <> toLogStr name <> "] "
      ft = case file of
        Just f -> LogFile (FileLogSpec f (toInteger maxSize) (fromIntegral rotateHistory)) $ fromIntegral bufferSize
        _      -> LogStdout $ fromIntegral bufferSize
  (l,close) <- newTimedFastLogger tc ft
  return (LogFunc (toLogger ln l) close)
  where
    toLogger xn f Loc{..} _ ll s = do
      lc <- level
      when (lc <= ll) $ f $ \t ->
        let locate = if ll /= LevelError then "" else " @" <> toLogStr loc_filename <> toLogStr (show loc_start)
        in toLogStr t <> " " <> toStr ll <> xn <> toLogStr loc_module <> locate <> " - " <> s <> "\n"

-- | Add additional trace info into log.
addTrace :: Text -> LogFunc -> LogFunc
addTrace trace lf = lf { logfunc = \a b c d -> let p = "[" <> toLogStr trace <> "] " in logfunc lf a b c (p <> d) }

-- | Plugin providing a logging function.
pluginLogger
  :: (MonadIO m, MonadCatch m, HasSalak cxt)
  => Text -- ^ Application name will be logged in log.
  -> Plugin cxt m LogFunc
pluginLogger name = do
  lc <- require "logging"
  bracketP (liftIO $ newLogger name lc) (\LogFunc{..} -> liftIO logend)
