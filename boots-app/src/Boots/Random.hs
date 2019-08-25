{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
module Boots.Random(
    RD(..)
  , HasRandom(..)
  , makeIORefRD
  , MonadRandom(..)
  , hex32
  , hex64
  ) where

import           Boots.App.Internal
import           Boots.Vault
import           Control.Monad.Factory
import           Data.IORef
import           Data.String
import           Data.Tuple
import           Data.Word
import           Lens.Micro
import           Lens.Micro.Extras
import           Numeric                (showHex)
import           System.Random.SplitMix

data RD = RD { unRD :: forall a. (SMGen -> (a, SMGen)) -> IO a }

class HasRandom env where
  askRandom :: Lens' env (VaultVal RD)

instance HasRandom (VaultVal RD) where
  askRandom = id
  {-# INLINE askRandom #-}

{-# INLINE makeIORefRD #-}
makeIORefRD :: SMGen -> IO RD
makeIORefRD seed = newIORef seed >>= \ref -> return (RD $ \f -> atomicModifyIORef' ref (swap.f))

class Monad m => MonadRandom env m | m -> env where
  nextW64   :: m Word64
  nextSplit :: m RD

hex64 :: IsString a => Word64 -> a
hex64 i = fromString $ let x = showHex i "" in replicate (16 - length x) '0' ++ x
{-# INLINE hex64 #-}

hex32 :: IsString a => Word64 -> a
hex32 i = fromString $ let x = showHex i "" in drop 8 $ replicate (16 - length x) '0' ++ x
{-# INLINE hex32 #-}

instance (HasRandom env, MonadMask n, MonadIO n) => MonadRandom env (Factory n env) where
  nextW64 = do
    rd <- asksEnv (view askRandom)
    liftIO $ unRD (readVaultDef rd) nextWord64
  {-# INLINE nextW64 #-}
  nextSplit = do
    rd <- asksEnv (view askRandom)
    liftIO $ unRD (readVaultDef rd) splitSMGen >>= makeIORefRD
  {-# INLINE nextSplit #-}

instance (HasVault env, HasRandom env, MonadIO n) => MonadRandom env (AppT env n) where
  nextW64 = do
    rd <- asks (view askRandom)
    liftIO $ unRD (readVaultDef rd) nextWord64
  {-# INLINE nextW64 #-}
  nextSplit = do
    rd <- asks (view askRandom)
    liftIO $ unRD (readVaultDef rd) splitSMGen >>= makeIORefRD
  {-# INLINE nextSplit #-}
