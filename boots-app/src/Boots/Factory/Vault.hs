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

class HasVault cxt env where
  askVault :: Lens' env (VaultRef cxt)

instance HasVault cxt (VaultRef cxt) where
  askVault = id

modifyVault :: (HasVault cxt env, MonadIO n) => (L.Vault -> cxt -> cxt) -> Factory n env ()
modifyVault f = asks (view askVault) >>= modifyVaultRef f

modifyVaultRef :: MonadIO n => (L.Vault -> env -> env) -> VaultRef env -> n ()
modifyVaultRef f ref = liftIO $ modifyMVar_ ref $ \g -> return $ \v -> g v . f v

runVault :: (MonadIO m, HasVault env env) => env -> L.Vault -> AppT env m a -> m a
runVault env v ma = do
  f <- liftIO $ readMVar $ view askVault env
  runAppT (f v env) ma
