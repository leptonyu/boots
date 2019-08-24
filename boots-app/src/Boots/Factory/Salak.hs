{-# LANGUAGE TypeSynonymInstances #-}
module Boots.Factory.Salak(
    HasSalak(..)
  , buildSalak
  -- ** Configuration Functions
  , MonadSalak(..)
  ) where

import           Boots.App.Internal
import           Control.Monad.Factory
import           Lens.Micro
import           Lens.Micro.Extras
import           Salak
import           Salak.Yaml

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

buildSalak :: (MonadIO m, MonadCatch m) => String -> Factory m () Salak
buildSalak name = liftIO $ runSalakWithYaml name askSourcePack
