{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
module Boots.Factory.Logger(
    HasLogger(..)
  , LogConfig(..)
  , LogFunc(..)
  , traceVault
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
import           Boots.Factory.Salak
import           Boots.Factory.Vault
import           Control.Concurrent
import           Control.Exception              (SomeException, catch)
import           Control.Monad
import           Control.Monad.Factory
import           Control.Monad.Logger.CallStack
import           Data.Default
import           Data.Int
import           Data.Text                      (Text, toLower, unpack)
import qualified Data.Vault.Lazy                as L
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
  {-# INLINE askLogLevel #-}

instance HasLogger LogFunc where
  askLogger = id
  {-# INLINE askLogger #-}

instance (MonadIO m, MonadMask m, HasLogger env) => MonadLogger (Factory m env) where
  monadLoggerLog a b c d = do
    LogFunc{..} <- asksEnv (view askLogger)
    liftIO $ logfunc a b c (toLogStr d)
  {-# INLINE monadLoggerLog #-}

instance (MonadIO m, HasLogger env) => MonadLogger (AppT env m) where
  monadLoggerLog a b c d = do
    LogFunc{..} <- asks (view askLogger)
    liftIO $ logfunc a b c (toLogStr d)
  {-# INLINE monadLoggerLog #-}

instance (MonadIO m, MonadMask m, HasLogger env) => MonadLoggerIO (Factory m env) where
  askLoggerIO = logfunc <$> asksEnv (view askLogger)
  {-# INLINE askLoggerIO #-}

instance (MonadIO m, HasLogger env) => MonadLoggerIO (AppT env m) where
  askLoggerIO = logfunc <$> asks (view askLogger)
  {-# INLINE askLoggerIO #-}

instance FromProp m LogLevel where
  fromProp = readEnum (fromEnumProp.toLower)
    where
      fromEnumProp "debug" = Right   LevelDebug
      fromEnumProp "info"  = Right   LevelInfo
      fromEnumProp "warn"  = Right   LevelWarn
      fromEnumProp "error" = Right   LevelError
      fromEnumProp u       = Left $ "unknown level: " ++ unpack u
      {-# INLINE fromEnumProp #-}
  {-# INLINE fromProp #-}

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
  {-# INLINE fromProp #-}

data LogFunc = LogFunc
  { logfunc :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  , logend  :: IO ()
  , logLvl  :: Writable LogLevel
  , logKey  :: L.Key Text
  , logFail :: IO Int64
  , logChan :: Chan (IO ())
  }

newLogger :: Text -> LogConfig -> IO LogFunc
newLogger name LogConfig{..} = do
  tc            <- newTimeCache "%Y-%m-%d %T"
  let ln = " [" <> toLogStr name <> "] "
      ft = case file of
        Just f -> LogFile (FileLogSpec f (toInteger maxSize) (fromIntegral rotateHistory)) $ fromIntegral bufferSize
        _      -> LogStdout $ fromIntegral bufferSize
  logLvl     <- toWritable level
  logKey     <- L.newKey
  logFailM   <- newMVar 0
  logChan    <- newChan
  _ <- forkIO $ forever $ join $ readChan logChan
  (l,logend) <- newTimedFastLogger tc ft
  let
    logFail = readMVar logFailM
    logfunc a b c d = writeChan logChan $ toLogger a b c d `catch` catchE
    catchE (_ :: SomeException) = modifyMVar_ logFailM (return . (+1))
    toLogger Loc{..} _ ll s = do
      lc <- getWritable logLvl
      when (lc <= ll) $ l $ \t ->
        let locate = if ll /= LevelError then "" else " @" <> toLogStr loc_filename <> toLogStr (show loc_start)
        in toLogStr t <> " " <> toStr ll <> ln <> toLogStr loc_module <> locate <> " - " <> s <> "\n"

  return (LogFunc{..})

-- | Add additional trace info into log.
traceVault :: L.Vault -> LogFunc -> LogFunc
traceVault v LogFunc{..} = LogFunc { logfunc = \a b c d -> logfunc a b c (go d), .. }
  where
    go :: LogStr -> LogStr
    go d = maybe d (\p -> "[" <> toLogStr p <> "] " <> d) $ L.lookup logKey v
    {-# INLINE go #-}
{-# INLINE traceVault #-}

-- | Add additional trace info into log.
addTrace :: Maybe Text -> LogFunc -> L.Vault -> L.Vault
addTrace (Just msg) LogFunc{..} v =
  let mt = L.lookup logKey v
  in case mt of
    Just m -> L.insert logKey (m <> "," <> msg) v
    _      -> L.insert logKey msg v
addTrace _ _ v = v
{-# INLINE addTrace #-}

buildLogger
  :: forall cxt m env
  . ( MonadIO m
    , MonadMask m
    , HasLogger cxt
    , HasVault cxt env)
  => Salak -> Text -> Factory m env LogFunc
buildLogger s name = do
  lc   <- within s $ require "logging"
  modifyVault @cxt $ over askLogger . traceVault
  produce (liftIO $ newLogger name lc) (\LogFunc{..} -> liftIO logend)
