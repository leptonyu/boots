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
  askSourcePack :: Lens' env SourcePack

instance HasSalak SourcePack where
  askSourcePack = id

instance (HasSalak env, Monad m) => MonadSalak (Factory m env) where
  askSalak = asks (view askSourcePack)

instance (HasSalak env, Monad m) => MonadSalak (AppT env m) where
  askSalak = asks (view askSourcePack)

buildSalak :: (MonadIO m, MonadCatch m) => String -> Factory m () Salak
buildSalak name = offer $ runSalakWithYaml name askSalak
