{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
module Boots.Factory.Application(
  -- ** Application
    HasApp(..)
  , AppEnv(..)
  , askExt
  , buildApp
  ) where

import           Boots.Factory.Logger
import           Boots.Factory.Salak
import           Boots.Health
import           Boots.Prelude
import           Boots.Random
import           Control.Concurrent    (setNumCapabilities)
import           Control.Monad.Factory
import qualified Data.ByteString       as B
import           Data.IORef
import           Data.Maybe
import           Data.Text             (Text)
import           Data.Text.Encoding
import           Data.Version          (Version)
import           Salak
import           Salak.Yaml

-- | Environment values with `AppEnv`.
class HasApp env ext where
  askApp :: Lens' env (AppEnv ext)

instance HasApp (AppEnv ext) ext where
  askApp = id
  {-# INLINE askApp #-}
instance HasLogger (AppEnv ext) where
  askLogger = lens logFunc (\x y -> x {logFunc = y})
  {-# INLINE askLogger #-}
instance HasSalak (AppEnv ext) where
  askSalak = lens configure (\x y -> x {configure = y})
  {-# INLINE askSalak #-}
instance HasRandom (AppEnv ext) where
  askRandom = lens randSeed (\x y -> x {randSeed = y})
  {-# INLINE askRandom #-}
instance HasHealth (AppEnv ext) where
  askHealth = lens health (\x y -> x {health = y})
  {-# INLINE askHealth #-}

-- | Application environment.
data AppEnv ext = AppEnv
  { name       :: Text      -- ^ Service name.
  , instanceId :: Text      -- ^ Service instance id.
  , version    :: Version   -- ^ Service version.
  , logFunc    :: LogFunc   -- ^ Logging function.
  , configure  :: Salak     -- ^ Configuration function.
  , randSeed   :: RD        -- ^ Random seed.
  , health     :: IO Health -- ^ Health check.
  , ext        :: ext
  }

askExt :: Lens' (AppEnv ext) ext
askExt = lens ext (\x y -> x {ext = y})

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
    <*> "random.type" .?= RDIORef

-- | Factory used to build `AppEnv`.
buildApp
  :: (MonadIO m, MonadMask m)
  => String
  -> Version
  -> ParseCommandLine
  -> ext
  -> Factory m () (AppEnv ext)
buildApp confName version mcli ext = do
  mv        <- liftIO $ newIORef []
  -- Initialize salak
  configure <- liftIO $ runSalak def
      { configName = confName
      , loggerF = \c s -> modifyIORef' mv ((c,s):)
      , loadExt = loadByExt YAML
      , commandLine = mcli
      } askSourcePack
  -- Read application name
  within configure $ do
    AppConfig{..} <- require "application"
    liftIO $ whenJust numCapabilities setNumCapabilities
    let name = fromMaybe (fromString confName) appName
    -- Generate instanceid
    randSeed   <- liftIO $ newRD randomType
    instanceId <- liftIO $ decodeUtf8 . B.take 8 . hex64 <$> unRD randSeed nextWord64
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

