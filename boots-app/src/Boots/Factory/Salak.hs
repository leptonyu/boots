{-# LANGUAGE TypeSynonymInstances #-}
module Boots.Factory.Salak(
  -- ** Configuration
    HasSalak(..)
  , MonadSalak(..)
  , buildSalak
  -- *** Utilities
  , tryBuildByKey
  ) where

import           Boots.App.Internal
import           Boots.Prelude
import           Control.Monad.Factory
import           Data.Maybe
import           Data.Text             (Text)
import           Salak
import           Salak.Yaml

-- | Environment values with `Salak`.
class HasSalak env where
  askSalak :: Lens' env Salak

instance HasSalak Salak where
  askSalak = id
  {-# INLINE askSalak #-}

instance (HasSalak env, MonadMask m) => MonadSalak (Factory m env) where
  askSourcePack = asksEnv (view askSalak)
  {-# INLINE askSourcePack #-}

instance (HasSalak env, Monad m) => MonadSalak (AppT env m) where
  askSourcePack = asks (view askSalak)
  {-# INLINE askSourcePack #-}

-- | Factory which loads configurations, and produces a configuration instance.
buildSalak :: (MonadIO m, MonadCatch m) => String -> Factory m () Salak
buildSalak name = liftIO $ runSalakWithYaml name askSourcePack

-- | A helper function used for try to build a factory if configuration set true.
tryBuildByKey
  ::(MonadMask m, MonadIO m, HasSalak env)
  => Bool -- ^ Default value.
  -> Text -- ^ Configuration key.
  -> Factory m env () -- ^ Target factory.
  -> Factory m env () -- ^ Launch the target factory if configuration is setted to be true.
tryBuildByKey defBool key fac = do
  b <- fromMaybe defBool <$> require key
  when b fac
