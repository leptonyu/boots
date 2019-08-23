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
  , MonadReader(..)
  , asks
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Data.Menshen

-- | Application monad transformation.
newtype AppT cxt m a = AppT { unAppT :: ReaderT cxt m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadReader cxt, MonadIO, MonadThrow, MonadCatch, MonadMask)

-- | Simple IO monad.
type App cxt = AppT cxt IO

-- | Run application monad transformation.
runAppT :: cxt -> AppT cxt m a -> m a
runAppT cxt ma = runReaderT (unAppT ma) cxt
{-# INLINE runAppT #-}

instance MonadUnliftIO m => MonadUnliftIO (AppT cxt m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = AppT $ ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . runAppT r))
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    AppT $ ReaderT $ \r ->
    withRunInIO $ \run ->
    inner (run . runAppT r)

instance MonadThrow m => HasValid (AppT cxt m)

