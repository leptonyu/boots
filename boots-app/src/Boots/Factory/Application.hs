{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
module Boots.Factory.Application(
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

class HasApp env where
  askApp :: Lens' env AppEnv

instance HasApp AppEnv where
  askApp = id
  {-# INLINE askApp #-}
instance HasLogger AppEnv where
  askLogger = lens logF (\x y -> x {logF = y})
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

data AppEnv = AppEnv
  { name       :: Text    -- ^ Service name.
  , instanceId :: Text    -- ^ Instance id.
  , version    :: Version -- ^ Service version.
  , logF       :: LogFunc
  , configure  :: Salak
  , randSeed   :: RD -- ^ Random seed
  , health     :: IO Health
  }

buildApp :: (MonadIO m, MonadMask m) => String -> Version -> Factory m () AppEnv
buildApp confName version = do
  mv        <- liftIO $ newIORef []
  -- Initialize salak
  configure <- liftIO $ runSalak def
      { configName = confName
      , loggerF = \c s -> modifyIORef' mv ((c,s):)
      , loadExt = loadByExt YAML
      } askSourcePack
  -- Read application name
  within configure $ do
    mayi       <- require "application.num-apabilities"
    liftIO $ whenJust mayi setNumCapabilities
    name       <- fromMaybe (fromString confName) <$> require "application.name"
    -- Generate instanceid
    tp         <- fromMaybe RDMVar                <$> require "application.random.type"
    randSeed   <- liftIO $ newRD tp
    instanceId <- liftIO $ hex32 <$> unRD randSeed nextWord64
    -- Initialize logger
    logF       <- buildLogger (name <> "," <> instanceId)
    -- Consume logs from salak
    let
      health = emptyHealth
      lf c s = logCS c LevelTrace (toLogStr s) logF
    liftIO $ atomicModifyIORef' mv ([],) >>= sequence_ . reverse . fmap (uncurry lf)
    -- Config new logger to salak
    setLogF lf
    return AppEnv{..}

