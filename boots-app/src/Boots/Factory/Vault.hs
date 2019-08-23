module Boots.Factory.Vault where

import           Boots.App.Internal
import           Control.Monad.Factory
import qualified Data.Vault.Lazy       as L
import           Lens.Micro
import           Lens.Micro.Extras

newtype VaultRef env = VaultRef (L.Vault -> env -> env)

class HasVault cxt env | env -> cxt where
  askVault :: Lens' env (VaultRef cxt)

instance HasVault cxt (VaultRef cxt) where
  askVault = id
  {-# INLINE askVault #-}

modifyContext
  :: (HasVault cxt env, MonadMask n, MonadIO n)
  => (Maybe a -> cxt -> cxt)
  -> Factory n env (L.Key a)
modifyContext f = do
  key <- liftIO L.newKey
  modifyVault $ f . L.lookup key
  return key
{-# INLINE modifyContext #-}

modifyVault :: (HasVault cxt env, MonadMask n) => (L.Vault -> cxt -> cxt) -> Factory n env ()
modifyVault f = modifyEnv $ over askVault $ \(VaultRef vr) -> VaultRef $ \v -> f v . vr v
{-# INLINE modifyVault #-}

runVault :: (MonadIO m, HasVault env env) => env -> L.Vault -> AppT env m a -> m a
runVault env v ma = do
  let VaultRef f = view askVault env
  runAppT (f v env) ma
{-# INLINE runVault #-}
