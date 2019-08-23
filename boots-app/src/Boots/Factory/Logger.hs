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
import           Boots.Factory
import           Boots.Factory.Salak
import           Boots.Factory.Vault
import           Control.Concurrent.MVar
import           Control.Exception              (SomeException, catch)
import           Control.Monad
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

instance (MonadIO m, HasLogger env) => MonadLogger (Factory m env) where
  monadLoggerLog a b c d = do
    LogFunc{..} <- asks (view askLogger)
    liftIO $ logfunc a b c (toLogStr d)
  {-# INLINE monadLoggerLog #-}

instance (MonadIO m, HasLogger env) => MonadLogger (AppT env m) where
  monadLoggerLog a b c d = do
    LogFunc{..} <- asks (view askLogger)
    liftIO $ logfunc a b c (toLogStr d)
  {-# INLINE monadLoggerLog #-}

instance (MonadIO m, HasLogger env) => MonadLoggerIO (Factory m env) where
  askLoggerIO = logfunc <$> asks (view askLogger)
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
  , logFail :: MVar Int64
  }

newLogger :: Text -> LogConfig -> IO LogFunc
newLogger name LogConfig{..} = do
  tc            <- newTimeCache "%Y-%m-%d %T"
  let ln = " [" <> toLogStr name <> "] "
      ft = case file of
        Just f -> LogFile (FileLogSpec f (toInteger maxSize) (fromIntegral rotateHistory)) $ fromIntegral bufferSize
        _      -> LogStdout $ fromIntegral bufferSize
  (l,logend) <- newTimedFastLogger tc ft
  logLvl     <- toWritable level
  logKey     <- L.newKey
  logFail    <- newMVar 0
  let logfunc a b c d = toLogger logLvl ln l a b c d `catch` \(_ :: SomeException) -> modifyMVar_ logFail (return . (+1))
  return (LogFunc{..})
  where
    {-# INLINE toLogger #-}
    toLogger logLvl ln f Loc{..} _ ll s = do
      lc <- getWritable logLvl
      when (lc <= ll) $ f $ \t ->
        let locate = if ll /= LevelError then "" else " @" <> toLogStr loc_filename <> toLogStr (show loc_start)
        in toLogStr t <> " " <> toStr ll <> ln <> toLogStr loc_module <> locate <> " - " <> s <> "\n"

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
  :: (MonadIO m, MonadMask m, HasSalak env, HasLogger cxt)
  => VaultRef cxt -> Text -> Factory m env LogFunc
buildLogger vf name = do
  lc  <- require "logging"
  modifyVaultRef (over askLogger . traceVault) vf
  bracket (liftIO $ newLogger name lc) (\LogFunc{..} -> liftIO logend)
