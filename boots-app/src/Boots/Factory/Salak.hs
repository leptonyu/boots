{-# LANGUAGE TypeApplications #-}
module Boots.Factory.Salak(
    HasSalak(..)
  , buildSalak
  -- ** Configuration Functions
  , MonadSalak(..)
  ) where

import           Boots.App.Internal
import           Boots.Factory
import           Lens.Micro
import           Lens.Micro.Extras
import           Salak
import           Salak.Yaml

class HasSalak env where
  askSalak :: Lens' env Salak

instance HasSalak Salak where
  askSalak = id
  {-# INLINE askSalak #-}

instance (HasSalak env, Monad m) => MonadSalak (Factory m env) where
  askSourcePack = gets (view askSalak)
  {-# INLINE askSourcePack #-}

instance (HasSalak env, Monad m) => MonadSalak (AppT env m) where
  askSourcePack = asks (view askSalak)
  {-# INLINE askSourcePack #-}

buildSalak :: (MonadIO m, MonadCatch m) => String -> Factory m () Salak
buildSalak name = offer $ runSalakWithYaml name askSourcePack
