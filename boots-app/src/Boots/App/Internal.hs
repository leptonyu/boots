{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module:      Boots.App.Internal
-- Copyright:   2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module defines a generic application monad transformation.
--
module Boots.App.Internal(
    AppT
  , App
  , runAppT
  , withAppT
  , MonadReader(..)
  , asks
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Unsafe.Coerce           (unsafeCoerce)

-- | Application monad transformation.
newtype AppT env m a = AppT { unAppT :: ReaderT env m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadReader env, MonadIO, MonadThrow, MonadCatch, MonadMask)

-- | Simple IO monad.
type App env = AppT env IO

-- | Run application monad transformation.
runAppT :: env -> AppT env m a -> m a
runAppT env ma = runReaderT (unAppT ma) env
{-# INLINE runAppT #-}

-- | Execute a computation in a modified environment.
withAppT :: (env -> env) -> AppT env m a -> AppT env m a
withAppT = unsafeCoerce withReaderT
{-# INLINE withAppT #-}

instance MonadUnliftIO m => MonadUnliftIO (AppT env m) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    AppT $ ReaderT $ \r ->
    withRunInIO $ \run ->
    inner (run . runAppT r)

