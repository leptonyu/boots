module Boots.Factory.Logger(
    HasLogger(..)
  , LogConfig(..)
  , LogFunc
  , addTrace
  , buildLogger
  -- ** Log Functions
  , MonadLogger(..)
  , MonadLoggerIO(..)
  , runLoggingT
  , logInfo
  , logDebug
  , logError
  , logWarn
  , logOther
  , LogLevel(..)
  ) where

import           Boots.App.Internal
import           Boots.Factory
import           Boots.Factory.Salak
import           Control.Monad
import           Control.Monad.Logger.CallStack
import           Data.Default
import           Data.Text                      (Text, toLower, unpack)
import           Data.Word
import           Lens.Micro
import           Lens.Micro.Extras
import           Salak
import           System.Log.FastLogger

-- | Environment providing a logging function.
class HasLogger env where
  askLogger :: Lens' env LogFunc
  askLogLevel :: Lens' env (Writable LogLevel)
  askLogLevel = askLogger . lens logLvl (\x y -> x { logLvl = y })

instance HasLogger LogFunc where
  askLogger = id

instance (MonadIO m, HasLogger env) => MonadLogger (Factory m env) where
  monadLoggerLog a b c d = do
    LogFunc{..} <- asks (view askLogger)
    liftIO $ logfunc a b c (toLogStr d)

instance (MonadIO m, HasLogger env) => MonadLogger (AppT env m) where
  monadLoggerLog a b c d = do
    LogFunc{..} <- asks (view askLogger)
    liftIO $ logfunc a b c (toLogStr d)

instance (MonadIO m, HasLogger env) => MonadLoggerIO (Factory m env) where
  askLoggerIO = logfunc <$> asks (view askLogger)

instance (MonadIO m, HasLogger env) => MonadLoggerIO (AppT env m) where
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
  { bufferSize    :: !Word16         -- ^ Logger buffer size.
  , file          :: !(Maybe FilePath) -- ^ Logger file path.
  , maxSize       :: !Word32         -- ^ Max logger file size.
  , rotateHistory :: !Word16         -- ^ Max number of logger files should be reserved.
  , level         :: !(IO LogLevel)    -- ^ Log level to show.
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
  , logLvl  :: Writable LogLevel
  }

newLogger :: Text -> LogConfig -> IO LogFunc
newLogger name LogConfig{..} = do
  tc            <- newTimeCache "%Y-%m-%d %T"
  let ln = " [" <> toLogStr name <> "] "
      ft = case file of
        Just f -> LogFile (FileLogSpec f (toInteger maxSize) (fromIntegral rotateHistory)) $ fromIntegral bufferSize
        _      -> LogStdout $ fromIntegral bufferSize
  (l,close) <- newTimedFastLogger tc ft
  lvl       <- toWritable level
  return (LogFunc (toLogger lvl ln l) close lvl)
  where
    toLogger lvl xn f Loc{..} _ ll s = do
      lc <- getWritable lvl
      when (lc <= ll) $ f $ \t ->
        let locate = if ll /= LevelError then "" else " @" <> toLogStr loc_filename <> toLogStr (show loc_start)
        in toLogStr t <> " " <> toStr ll <> xn <> toLogStr loc_module <> locate <> " - " <> s <> "\n"

-- | Add additional trace info into log.
addTrace :: Text -> LogFunc -> LogFunc
addTrace trace lf = lf { logfunc = \a b c d -> let p = "[" <> toLogStr trace <> "] " in logfunc lf a b c (p <> d) }


buildLogger
  :: (MonadIO m, MonadCatch m, HasSalak env)
  => Text -> Factory m env LogFunc
buildLogger name = do
  lc <- require "logging"
  bracket (liftIO $ newLogger name lc) (\LogFunc{..} -> liftIO logend)
