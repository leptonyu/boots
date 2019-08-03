-- |
-- Module:      Boots.Internal.Plugin
-- Copyright:   2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module defines a generic application plugin used when booting application.
--
module Boots.Internal.Plugin(
    boot
  , Plugin
  , runPlugin
  , promote
  , union
  , withPlugin
  , mapPlugin
  , isoPlugin
  , bracketP
  , wrapP
  , delayP
  , Monoid(..)
  ) where

import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Reader
import           Data.Monoid

-- | Plugin generates component @u@ with the context of component @i@ running in monad @m@.
newtype Plugin i m u = Plugin { unPlugin :: ReaderT i (ContT () m) u }
  deriving (Functor, Applicative, Monad, MonadReader i, MonadIO)

-- | Run plugin in given context @i@.
runPlugin :: i -> Plugin i m u -> (u -> m ()) -> m ()
runPlugin i pma = runContT (runReaderT (unPlugin pma) i)

-- | Run application only in plugin.
boot :: Monad m => Plugin () m (m ()) -> m ()
boot plugin = runPlugin () plugin id

instance MonadTrans (Plugin i) where
  lift = Plugin . lift . lift

instance MonadThrow m => MonadThrow (Plugin i m) where
  throwM = lift . throwM

instance Monad m => MonadCont (Plugin i m) where
  callCC a = do
    i <- ask
    Plugin . lift . ContT . runPlugin i $ callCC a

instance Semigroup (Plugin i n i) where
  (<>) = union

instance Monoid (Plugin i n i) where
  mempty = ask

-- | Promote a plugin into another.
promote :: i -> Plugin i m u -> Plugin x m u
promote i pimu = Plugin $ lift $ ContT (runPlugin i pimu)

union :: Plugin i m j -> Plugin j m k -> Plugin i m k
union pij pjk = pij >>= (`promote` pjk)

-- | Convert a plugin into another.
withPlugin :: (i -> j) -> Plugin j m u -> Plugin i m u
withPlugin f = Plugin . withReaderT f . unPlugin

-- | Transform a plugin with monad @n@ to a plugin with monad @m@.
isoPlugin :: (m () -> n ()) -> (n () -> m ()) -> Plugin i n u -> Plugin i m u
isoPlugin f g = Plugin . mapReaderT go . unPlugin
  where
    go (ContT fnc) = ContT $ \mc -> g $ fnc (f . mc)

-- | Apply a function to transform the result of a continuation-passing computation.
mapPlugin :: (m () -> m ()) -> Plugin i m u -> Plugin i m u
mapPlugin f = Plugin . mapReaderT (mapContT f) . unPlugin

-- | Warp plugin.
wrapP :: ((u -> m ()) -> m ()) -> Plugin i m u
wrapP = Plugin . lift . ContT

-- | Create bracket style plugin, used for manage resources, which need to open and close.
--
-- A simple example:
--
-- >>> res = bracketP (putStrLn "open") (const $ putStrLn "close")
-- >>> runPlugin () res (const $ putStrLn "using")
-- open
-- using
-- close
bracketP
  :: forall m i u. MonadCatch m
  => m u          -- ^ Open resource.
  -> (u -> m ())  -- ^ Close resource.
  -> Plugin i m u -- ^ Resource plugin.
bracketP op cl = Plugin $ lift $ withContT go (lift op)
  where
    {-# INLINE go #-}
    go f u = do
      v <- try $ f u
      _ <- try $ cl u :: m (Either SomeException ())
      case v of
        Left  e -> throwM (e :: SomeException)
        Right x -> return x

-- | Add a delayed action to plugin.
delayP :: MonadCatch m => m () -> Plugin i m ()
delayP ma = bracketP (return ()) (const ma)


