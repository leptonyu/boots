{-# LANGUAGE RankNTypes #-}
module Boots.Vault(
    VaultRef
  , newVR
  , lensVR
  , valDef
  , readVR
  , writeVR
  , modifyVR
  , mapVR
  ) where

import           Data.Maybe
import qualified Data.Vault.Lazy as L
import           Lens.Micro

data VaultRef a = VaultRef
  { valDef :: a
  , valKey :: L.Key a
  }

lensVR :: Lens' (VaultRef a) a
lensVR = lens valDef (\VaultRef{..} a -> VaultRef{ valDef = a, ..})

newVR :: a -> IO (VaultRef a)
newVR valDef = do
  valKey <- L.newKey
  return VaultRef{..}

readVR :: VaultRef a -> L.Vault -> a
readVR VaultRef{..} = fromMaybe valDef . L.lookup valKey

writeVR :: VaultRef a -> a -> L.Vault -> L.Vault
writeVR VaultRef{..} = L.insert valKey

modifyVR :: VaultRef a -> (a -> a) -> L.Vault -> L.Vault
modifyVR VaultRef{..} f = L.adjust f valKey

mapVR :: (a -> a) -> VaultRef a -> VaultRef a
mapVR f VaultRef{..} = VaultRef{ valDef = f valDef, ..}
