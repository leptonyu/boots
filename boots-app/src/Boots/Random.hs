{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
module Boots.Random(
    RD(..)
  , HasRandom(..)
  , newRD
  , makeRD
  , makeRD0
  , forkRD
  , MonadRandom(..)
  , hex32
  , hex64
  , nextWord64
  , splitSMGen
  ) where

import           Boots.App.Internal
import           Control.Monad.Factory
import           Data.IORef
import           Data.String
import           Data.Tuple
import           Foreign
import           Lens.Micro
import           Lens.Micro.Extras
import           Numeric                (showHex)
import           System.Random.SplitMix

data RD = RD { unRD :: forall a. (SMGen -> (a, SMGen)) -> IO a }

class HasRandom env where
  askRandom :: Lens' env RD

instance HasRandom RD where
  askRandom = id
  {-# INLINE askRandom #-}

{-# INLINE newRD #-}
newRD :: IO RD
newRD = initSMGen >>= makeRD

{-# INLINE makeRD #-}
makeRD :: SMGen -> IO RD
makeRD seed = newIORef seed >>= \ref -> return (RD $ \f -> atomicModifyIORef' ref (swap . f))

{-# INLINE makeRD0 #-}
makeRD0 :: SMGen -> (RD -> IO a) -> IO a
makeRD0 smg f = do
  let (seed, gamma) = unseedSMGen smg
  allocaArray 2 $ \ps -> do
    pokeArray ps [seed,gamma]
    f $ RD $ \func -> do
      [s0,g0] <- peekArray 2 ps
      let (a, smg2) = func $ seedSMGen s0 g0
          (s1,g1)   = unseedSMGen smg2
      pokeArray ps [s1,g1]
      return a

{-# INLINE forkRD #-}
forkRD :: RD -> IO RD
forkRD (RD f) = f splitSMGen >>= makeRD

class Monad m => MonadRandom env m | m -> env where
  nextW64   :: m Word64

hex64 :: IsString a => Word64 -> a
hex64 i = fromString $ let x = showHex i "" in replicate (16 - length x) '0' ++ x
{-# INLINE hex64 #-}

hex32 :: IsString a => Word64 -> a
hex32 i = fromString $ let x = showHex i "" in drop 8 $ replicate (16 - length x) '0' ++ x
{-# INLINE hex32 #-}

instance (HasRandom env, MonadMask n, MonadIO n) => MonadRandom env (Factory n env) where
  nextW64 = do
    vr <- asksEnv (view askRandom)
    liftIO $ unRD vr nextWord64
  {-# INLINE nextW64 #-}

instance (HasRandom env, MonadIO n) => MonadRandom env (AppT env n) where
  nextW64 = do
    vr <- asks (view askRandom)
    liftIO $ unRD vr nextWord64
  {-# INLINE nextW64 #-}
