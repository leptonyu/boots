{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
module Boots.Factory.Logger(
  -- ** Logger
    HasLogger(..)
  , LogConfig(..)
  , LogFunc(..)
  , addTrace
  , buildLogger
  -- *** Log Functions
  , logTrace
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logFatal
  , logCS
  , LogLevel(..)
  -- *** Reexport log functions
  , levelFromStr
  , ToLogStr(..)
  , LogStr
  ) where

import           Boots.App.Internal
import           Boots.Factory.Salak
import           Boots.Prelude
import           Control.Exception     (SomeException, catch)
import           Control.Monad.Factory
import           Data.Int
import           Data.IORef
import           Data.Text             (Text, toLower, unpack)
import           Data.Word
import           GHC.Stack
import           Salak
import           System.Log.FastLogger

-- | Log level.
data LogLevel
  = LevelTrace
  | LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  | LevelFatal
  deriving (Eq, Ord, Show)

-- | Environment providing a logging function.
class HasLogger env where
  askLogger :: Lens' env LogFunc

instance HasLogger LogFunc where
  askLogger = id
  {-# INLINE askLogger #-}

instance FromProp m LogLevel where
  fromProp = readEnum levelFromStr
  {-# INLINE fromProp #-}

-- | Parsing `LogLevel` from string.
{-# INLINE levelFromStr #-}
levelFromStr :: Text -> Either String LogLevel
levelFromStr = go . toLower
  where
    {-# INLINE go #-}
    go "trace" = Right   LevelTrace
    go "debug" = Right   LevelDebug
    go "info"  = Right   LevelInfo
    go "warn"  = Right   LevelWarn
    go "error" = Right   LevelError
    go "fatal" = Right   LevelFatal
    go u       = Left $ "unknown level: " ++ unpack u

{-# INLINE toStr #-}
toStr :: LogLevel -> LogStr
toStr LevelTrace = "TRACE"
toStr LevelDebug = "DEBUG"
toStr LevelInfo  = " INFO"
toStr LevelWarn  = " WARN"
toStr LevelError = "ERROR"
toStr LevelFatal = "FATAL"

-- | Logger configuation used to customizing `LogFunc`.
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
  {-# INLINE fromProp #-}

-- | A `Monad` which has the ability to log messages.
class (MonadIO m, HasLogger e) => MonadLog e m | m -> e where
  askLog :: m LogFunc

instance (MonadIO m, HasLogger env) => MonadLog env (AppT env m) where
  askLog = asks (view askLogger)

instance (MonadIO m, MonadMask m, HasLogger env) => MonadLog env (Factory m env) where
  askLog = asksEnv (view askLogger)

-- | Logs a `LevelTrace` message.
logTrace :: (MonadLog e m, HasCallStack) => LogStr -> m ()
logTrace s = askLog >>= liftIO . logCS callStack LevelTrace s

-- | Logs a `LevelDebug` message.
logDebug :: (MonadLog e m, HasCallStack) => LogStr -> m ()
logDebug s = askLog >>= liftIO . logCS callStack LevelDebug s

-- | Logs a `LevelInfo` message.
logInfo :: (MonadLog e m, HasCallStack) => LogStr -> m ()
logInfo s = askLog >>= liftIO . logCS callStack LevelInfo s

-- | Logs a `LevelWarn` message.
logWarn :: (MonadLog e m, HasCallStack) => LogStr -> m ()
logWarn s = askLog >>= liftIO . logCS callStack LevelWarn s

-- | Logs a `LevelError` message.
logError :: (MonadLog e m, HasCallStack) => LogStr -> m ()
logError s = askLog >>= liftIO . logCS callStack LevelError s

-- | Logs a `LevelFatal` message.
logFatal :: (MonadLog e m, HasCallStack) => LogStr -> m ()
logFatal s = askLog >>= liftIO . logCS callStack LevelFatal s

-- | Logs a message with location given by `CallStack`.
{-# INLINE logCS #-}
logCS :: CallStack -> LogLevel -> LogStr -> LogFunc -> IO ()
logCS cs ll ls lf = logfunc lf (go $ getCallStack cs) ll ls
  where
    {-# INLINE go #-}
    go ((_,loc):_) = loc
    go _           = def

instance Default SrcLoc where
  def = SrcLoc
    "<unknown>"
    "<unknown>"
    "<unknown>"
    0 0 0 0

-- | A closable logging function.
-- Also supporting change log level and count failed logs.
data LogFunc = LogFunc
  { logfunc :: SrcLoc -> LogLevel -> LogStr -> IO ()
  , logend  :: IO ()
  , logLvl  :: Writable LogLevel
  , logFail :: IO Int64
  }

-- | Log event.
data LogEvent = LogEvent
  { lloc   :: SrcLoc
  , llevel :: LogLevel
  , llog   :: LogStr
  , lname  :: LogStr
  , ltime  :: IO FormattedTime
  }

{-# INLINE runLog #-}
runLog :: (LogStr -> IO ()) -> Writable LogLevel -> LogEvent -> IO ()
runLog !lf !logLvl LogEvent{..} = do
  lc <- getWritable logLvl
  when (lc <= llevel) $ makeLog lloc >>= lf
  where
    {-# INLINE makeLog #-}
    makeLog SrcLoc{..} = do
      t <- ltime
      let locate = if llevel <= LevelWarn
            then ""
            else " @" <> toLogStr srcLocFile <> toLogStr (show srcLocStartCol)
      return
        $ toLogStr t
        <> " "
        <> toStr llevel
        <> lname
        <> toLogStr srcLocModule
        <> locate
        <> " - "
        <> llog
        <> "\n"

-- | Create a new `LogFunc`.
newLogger :: Text -> LogConfig -> IO LogFunc
newLogger name LogConfig{..} = do
  (logf,logend)  <- newFastLogger $ case file of
        Just f -> LogFile (FileLogSpec f (toInteger maxSize) (fromIntegral rotateHistory)) $ fromIntegral bufferSize
        _      -> LogStdout $ fromIntegral bufferSize
  ltime    <- newTimeCache "%Y-%m-%d %T"
  logLvl   <- toWritable level
  logFailM <- newIORef 0
  let
    {-# INLINE logFail #-}
    logFail = readIORef logFailM
    {-# INLINE lfail #-}
    lfail (_::SomeException) = atomicModifyIORef' logFailM (\a -> (a+1,()))
    {-# INLINE lname #-}
    lname = " [" <> toLogStr name <> "] "
    {-# INLINE logfunc #-}
    logfunc lloc llevel llog = runLog logf logLvl LogEvent {..} `catch` lfail
  return LogFunc{..}

-- | Add additional trace info into log.
{-# INLINE addTrace #-}
addTrace :: ToLogStr msg => msg -> LogFunc -> LogFunc
addTrace msg LogFunc{..} = LogFunc
  { logfunc = \a b c -> logfunc a b ("[" <> toLogStr msg <> "] " <> c)
  , ..}

-- | Factory which produces a `LogFunc`.
buildLogger
  :: ( MonadIO m
    , MonadMask m
    , HasSalak env)
  => Text -> Factory m env LogFunc
buildLogger name = do
  lc   <- require "logging"
  produce (liftIO $ newLogger name lc) (\LogFunc{..} -> liftIO logend)
