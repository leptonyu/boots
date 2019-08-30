{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
module Boots.Factory.Application(
  -- ** Application
    HasApp(..)
  , AppEnv(..)
  , buildApp
  ) where

import           Boots.Factory.Logger
import           Boots.Factory.Salak
import           Boots.Health
import           Boots.Prelude
import           Boots.Random
import           Control.Concurrent    (setNumCapabilities)
import           Control.Monad.Factory
import           Data.IORef
import           Data.Maybe
import           Data.Text             (Text)
import           Data.Version          (Version)
import           Salak
import           Salak.Yaml

-- | Environment values with `AppEnv`.
class HasApp env where
  askApp :: Lens' env AppEnv

instance HasApp AppEnv where
  askApp = id
  {-# INLINE askApp #-}
instance HasLogger AppEnv where
  askLogger = lens logFunc (\x y -> x {logFunc = y})
  {-# INLINE askLogger #-}
instance HasSalak AppEnv where
  askSalak = lens configure (\x y -> x {configure = y})
  {-# INLINE askSalak #-}
instance HasRandom AppEnv where
  askRandom = lens randSeed (\x y -> x {randSeed = y})
  {-# INLINE askRandom #-}
instance HasHealth AppEnv where
  askHealth = lens health (\x y -> x {health = y})
  {-# INLINE askHealth #-}

-- | Application environment.
data AppEnv = AppEnv
  { name       :: Text      -- ^ Service name.
  , instanceId :: Text      -- ^ Service instance id.
  , version    :: Version   -- ^ Service version.
  , logFunc    :: LogFunc   -- ^ Logging function.
  , configure  :: Salak     -- ^ Configuration function.
  , randSeed   :: RD        -- ^ Random seed.
  , health     :: IO Health -- ^ Health check.
  }

-- | Application configuration used for customizing `AppEnv`.
data AppConfig = AppConfig
  { appName         :: Maybe Text
  , numCapabilities :: Maybe Int
  , randomType      :: RDType
  }

instance FromProp m AppConfig where
  {-# INLINE fromProp #-}
  fromProp = AppConfig
    <$> "name"
    <*> "num-capabilities"
    <*> "random.type" .?= RDMVar

-- | Factory used to build `AppEnv`.
buildApp
  :: (MonadIO m, MonadMask m)
  => String
  -> Version
  -> Maybe ParseCommandLine
  -> Factory m () AppEnv
buildApp confName version mcli = do
  mv        <- liftIO $ newIORef []
  -- Initialize salak
  configure <- liftIO $ runSalak def
      { configName = confName
      , loggerF = \c s -> modifyIORef' mv ((c,s):)
      , loadExt = loadByExt YAML
      , commandLine = fromMaybe (commandLine def) mcli
      } askSourcePack
  -- Read application name
  within configure $ do
    AppConfig{..} <- require "application"
    liftIO $ whenJust numCapabilities setNumCapabilities
    let name = fromMaybe (fromString confName) appName
    -- Generate instanceid
    randSeed   <- liftIO $ newRD randomType
    instanceId <- liftIO $ hex32 <$> unRD randSeed nextWord64
    -- Initialize logger
    logFunc    <- buildLogger (name <> "," <> instanceId)
    -- Consume logs from salak
    let
      health = emptyHealth
      lf c s = logCS c LevelTrace (toLogStr s) logFunc
    liftIO $ atomicModifyIORef' mv ([],) >>= sequence_ . reverse . fmap (uncurry lf)
    -- Config new logger to salak
    setLogF lf
    return AppEnv{..}

