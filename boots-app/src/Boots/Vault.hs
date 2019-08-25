{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Boots.Vault(
    VaultVal
  , L.Vault
  , newVaultVal
  , readVaultDef
  , readVault
  , writeVault
  , mapVault
  , HasVault(..)
  ) where

import           Data.Maybe
import qualified Data.Vault.Lazy as L
import           Lens.Micro

data VaultVal a = VaultVal
  { valKey :: L.Key a
  , valDef :: a
  }

newVaultVal :: a -> IO (VaultVal a)
newVaultVal valDef = do
  valKey <- L.newKey
  return VaultVal{..}

readVaultDef :: VaultVal a -> a
readVaultDef = valDef

readVault :: VaultVal a -> L.Vault -> a
readVault VaultVal{..} v = fromMaybe valDef $ L.lookup valKey v

writeVault :: VaultVal a -> a -> L.Vault -> L.Vault
writeVault VaultVal{..}= L.insert valKey

mapVault :: (a -> a) -> VaultVal a -> VaultVal a
mapVault f VaultVal{..} = VaultVal{valDef = f valDef,..}

class HasVault env where
  askVault :: Lens' env L.Vault

instance HasVault L.Vault where
  askVault = id
