module Boots.Factory.Vault where

import           Boots.App.Internal
import           Boots.Factory
import           Control.Concurrent.MVar
import qualified Data.Vault.Lazy         as L
import           Lens.Micro
import           Lens.Micro.Extras

type VaultRef env = MVar (L.Vault -> env -> env)

newVaultRef :: IO (VaultRef env)
newVaultRef = newMVar $ const id
{-# INLINE newVaultRef #-}

class HasVault cxt env | env -> cxt where
  askVault :: Lens' env (VaultRef cxt)

instance HasVault cxt (VaultRef cxt) where
  askVault = id
  {-# INLINE askVault #-}


modifyContext :: (HasVault cxt env, MonadIO n) => (Maybe a -> cxt -> cxt) -> Factory n env (L.Key a)
modifyContext f = do
  key <- liftIO L.newKey
  modifyVault $ f . L.lookup key
  return key
{-# INLINE modifyContext #-}

modifyVault :: (HasVault cxt env, MonadIO n) => (L.Vault -> cxt -> cxt) -> Factory n env ()
modifyVault f = asks (view askVault) >>= modifyVaultRef f
{-# INLINE modifyVault #-}

modifyVaultRef :: MonadIO n => (L.Vault -> env -> env) -> VaultRef env -> n ()
modifyVaultRef f ref = liftIO $ modifyMVar_ ref $ \g -> return $ \v -> g v . f v
{-# INLINE modifyVaultRef #-}

runVault :: (MonadIO m, HasVault env env) => env -> L.Vault -> AppT env m a -> m a
runVault env v ma = do
  f <- liftIO $ readMVar $ view askVault env
  runAppT (f v env) ma
