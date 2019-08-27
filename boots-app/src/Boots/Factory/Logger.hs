{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeApplications       #-}
module Boots.Factory.Logger(
    HasLogger(..)
  , LogConfig(..)
  , LogFunc(..)
  , addTrace
  , buildLogger
  -- ** Log Functions
  , logTrace
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logFatal
  , logCS
  , ToLogStr(..)
  , LogStr
  , LogLevel(..)
  -- ** Reexport log functions
  , levelFromStr
  ) where

import           Boots.App.Internal
import           Boots.Factory.Salak
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception       (SomeException, catch, finally, mask_)
import           Control.Monad
import           Control.Monad.Factory
import           Data.Default
import           Data.Int
import           Data.IORef
import           Data.Text               (Text, toLower, unpack)
import           Data.Word
import           GHC.Stack
import           Lens.Micro
import           Lens.Micro.Extras
import           Salak
import           System.Log.FastLogger


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

-- | Logger config.
data LogConfig = LogConfig
  { bufferSize    :: !Word16         -- ^ Logger buffer size.
  , file          :: !(Maybe FilePath) -- ^ Logger file path.
  , maxSize       :: !Word32         -- ^ Max logger file size.
  , rotateHistory :: !Word16         -- ^ Max number of logger files should be reserved.
  , level         :: !(IO LogLevel)    -- ^ Log level to show.
  , asyncMode     :: !Bool
  }

instance Default LogConfig where
  def = LogConfig 4096 Nothing 10485760 256 (return LevelInfo) True

instance MonadIO m => FromProp m LogConfig where
  fromProp = LogConfig
    <$> "buffer-size" .?: bufferSize
    <*> "file"        .?: file
    <*> "max-size"    .?: maxSize
    <*> "max-history" .?: rotateHistory
    <*> "level"       .?: level
    <*> "async"       .?: asyncMode
  {-# INLINE fromProp #-}


class (MonadIO m, HasLogger e) => MonadLog e m | m -> e where
  askLog :: m LogFunc

instance (MonadIO m, HasLogger env) => MonadLog env (AppT env m) where
  askLog = asks (view askLogger)

instance (MonadIO m, MonadMask m, HasLogger env) => MonadLog env (Factory m env) where
  askLog = asksEnv (view askLogger)

logTrace :: (MonadLog e m, HasCallStack) => LogStr -> m ()
logTrace s = askLog >>= liftIO . logCS callStack LevelTrace s

logDebug :: (MonadLog e m, HasCallStack) => LogStr -> m ()
logDebug s = askLog >>= liftIO . logCS callStack LevelDebug s

logInfo :: (MonadLog e m, HasCallStack) => LogStr -> m ()
logInfo s = askLog >>= liftIO . logCS callStack LevelInfo s

logWarn :: (MonadLog e m, HasCallStack) => LogStr -> m ()
logWarn s = askLog >>= liftIO . logCS callStack LevelWarn s

logError :: (MonadLog e m, HasCallStack) => LogStr -> m ()
logError s = askLog >>= liftIO . logCS callStack LevelError s

logFatal :: (MonadLog e m, HasCallStack) => LogStr -> m ()
logFatal s = askLog >>= liftIO . logCS callStack LevelFatal s

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

data LogFunc = LogFunc
  { logfunc :: SrcLoc -> LogLevel -> LogStr -> IO ()
  , logend  :: IO ()
  , logLvl  :: Writable LogLevel
  , logFail :: IO Int64
  }

data LogEvent = LogEvent
  { lloc   :: SrcLoc
  , llevel :: LogLevel
  , llog   :: LogStr
  , lname  :: LogStr
  , ltime  :: IO FormattedTime
  }

{-# INLINE runLog #-}
runLog :: (LogStr -> IO ()) -> Writable LogLevel -> LogEvent -> IO ()
runLog !lf !logLvl !LogEvent{..} = do
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

{-# INLINE asyncLog #-}
asyncLog
  :: (LogStr -> IO ())
  -> Writable LogLevel
  -> (SomeException -> IO ())
  -> IO ()
  -> IO (LogEvent -> IO (), IO ())
asyncLog lf ll lfail le = do
  rc <- newChan -- First Channel
  b  <- newIORef True
  let
    loop rr ww = do
      xb <- readIORef b
      when xb $ do
        _ <- readChan rr >>= ww
        loop rr ww
    {-# INLINE leftc #-}
    leftc = mask_ (getChanContents rc >>= mapM_ (runLog lf ll))
  void $ forkIO $ loop rc (runLog lf ll)
  return
    ( writeChan rc
    , (modifyIORef' b (\_ -> False) >> (catch leftc lfail)) `finally` le
    )

newLogger :: Text -> LogConfig -> IO LogFunc
newLogger name LogConfig{..} = do
  (logf,close)  <- newFastLogger $ case file of
        Just f -> LogFile (FileLogSpec f (toInteger maxSize) (fromIntegral rotateHistory)) $ fromIntegral bufferSize
        _      -> LogStdout $ fromIntegral bufferSize
  ltime    <- newTimeCache "%Y-%m-%d %T"
  logLvl   <- toWritable level
  logFailM <- newMVar 0
  let
    {-# INLINE logFail #-}
    logFail = readMVar logFailM
    {-# INLINE lfail #-}
    lfail   = \(_::SomeException) -> modifyMVar_ logFailM (return . (+1))
    {-# INLINE lname #-}
    lname = " [" <> toLogStr name <> "] "
  (execLog,logend) <- if asyncMode
    then asyncLog logf logLvl lfail close
    else return (\e -> runLog logf logLvl e `catch` lfail, close)
  let
    {-# INLINE logfunc #-}
    logfunc ~lloc ~llevel ~llog = execLog LogEvent {..}
  return LogFunc{..}

-- | Add additional trace info into log.
{-# INLINE addTrace #-}
addTrace :: ToLogStr msg => msg -> LogFunc -> LogFunc
addTrace msg LogFunc{..} = LogFunc
  { logfunc = \a b c -> logfunc a b ("[" <> toLogStr msg <> "] " <> c)
  , ..}

buildLogger
  :: ( MonadIO m
    , MonadMask m
    , HasSalak env)
  => Text -> Factory m env LogFunc
buildLogger name = do
  lc   <- require "logging"
  produce (liftIO $ newLogger name lc) (\LogFunc{..} -> liftIO logend)
