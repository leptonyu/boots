{-# LANGUAGE CPP #-}
-- |
-- Module:      Boots.Factory
-- Copyright:   2019 Daniel YU
-- License:     MIT
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- IoC Monad in Haskell.
--
-- * Motivation
--
-- Simplify to create an application in Haskell.
--
-- When we decide to create an application using Haskell.
-- We may need using configurations, loggers as basic functions.
-- If this application needs storages, caches, etc.,
-- then we have to weaving the management of connection of these facilities into the application.
-- Connections need to be created before and be destroyed after using them.
-- There is a common strategy to manage connections, that is using `Control.Monad.Cont`.
-- Then we can encapsulate the management of connections separately.
-- For example, we can write a database plugin `Factory` @m@ @cxt@ @DBConnection@,
-- which can manage the database connections in monad @m@ with context @cxt@.
-- Context @cxt@ may be requested for getting configurations or logging functions.
-- When all the components of application are encapsulated by plugins, then running an application will be simplified.
--
-- * Factory
--
-- 'Factory' has an environment @env@, which provides anything needs by the factory. @component@ is the production of
-- the factory, it will be used by other 'Factory'. Finally to build a complete 'Factory' m () (m ()), which can be 'boot'.
--
-- For example:
--
-- > factory = do
-- >   log  <-  logFactory
-- >   conf <- confFactory
-- >   within (log, conf) $ do
-- >     a <- withFactory fst aFactory
-- >     b <- withFactory snd bFactory
-- >     polish AB{..}
-- >       [ xFactory
-- >       , yFactory
-- >       ] >>> bootFactory
module Boots.Factory(
  -- * Definition
    Factory
  -- ** Run functions
  , running
  , boot
  -- * Factory Construction
  -- ** With
  , withFactory
  , within
  -- ** Polish
  , polish
  -- ** Nature Transformation
  , natTrans
  -- ** Resource
  , wrap
  , bracket
  , offer
  , delay
  -- * Reexport Function
  -- ** Category Arrow
  , (C.>>>)
  , (C.<<<)
  -- ** Monoid Join
  , (<>)
  -- ** Other
  , MonadThrow(..)
  , MonadCatch
  , MonadMask
  , MonadReader(..)
  , asks
  , MonadIO(..)
  , lift
  ) where

import qualified Control.Category     as C
import           Control.Monad.Catch  hiding (bracket)
import           Control.Monad.Cont
import           Control.Monad.Reader
import           Unsafe.Coerce        (unsafeCoerce)
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif

-- | Factory defines how to generate a @component@ under the environment @env@ in monad @m@.
-- It is similar to IoC container in oop, @env@ will provide anything to be wanted to generate @component@.
--
newtype Factory m env component
  = Factory { unFactory :: ReaderT env (ContT () m) component }
  deriving (Functor, Applicative, Monad, MonadReader env, MonadIO)

instance MonadThrow m => MonadThrow (Factory m env) where
  {-# INLINE throwM #-}
  throwM = offer . throwM

instance Monad m => MonadCont (Factory m env) where
  {-# INLINE callCC #-}
  callCC a = do
    env <- ask
    wrap . running env $ callCC a

instance Semigroup (Factory m env env) where
  {-# INLINE (<>) #-}
  a <> b = a >>= (`within` b)

instance Monoid (Factory m env env) where
  {-# INLINE mempty #-}
  mempty = ask
  {-# INLINE mappend #-}
  mappend = (<>)

instance C.Category (Factory m) where
  {-# INLINE id #-}
  id  = ask
  {-# INLINE (.) #-}
  a . b = b >>= (`within` a)

-- | Running the factory.
running :: env -> Factory m env c -> (c -> m ()) -> m ()
running env pma = runContT (runReaderT (unFactory pma) env)
{-# INLINE running #-}

-- | Run the application using a specified factory.
boot :: Monad m => Factory m () (m ()) -> m ()
boot factory = running () factory id

-- | Switch factory environment.
withFactory :: (env' -> env) -> Factory m env component -> Factory m env' component
withFactory = unsafeCoerce withReaderT
{-# INLINE withFactory #-}

-- | Construct factory under @env@, and adapt it to fit another @env'@.
within :: env -> Factory m env component -> Factory m env' component
within = withFactory . const
{-# INLINE within #-}

-- | Polish @component@ by a sequence of 'Factory', and construct a unified one.
polish :: component -> [Factory m component component] -> Factory m env' component
polish env = within env . mconcat
{-# INLINE polish #-}

-- | Nature transform of one 'Factory' with monad @n@ into another with monad @m@.
natTrans :: (n () -> m ()) -> (m () -> n ()) -> Factory n env component -> Factory m env component
natTrans fnm fmn fac = do
  env <- ask
  wrap $ \fm -> fnm $ running env fac (fmn . fm)
{-# INLINE natTrans #-}

-- | Wrap raw procedure into a 'Factory'.
wrap :: ((c -> m ()) -> m ()) -> Factory m env c
wrap = Factory . lift . ContT
{-# INLINE wrap #-}

-- | Construct open-close resource into a 'Factory'.
bracket :: MonadMask m => m res -> (res -> m ()) -> Factory m env res
bracket open close = wrap $ \f -> mask $ \restore -> do
  res <- open
  a   <- try $ restore $ f res
  b   <- try $ close res
  go a b
  where
    go (Left e) _ = throwM (e :: SomeException)
    go _ (Left e) = throwM (e :: SomeException)
    go _ _        = return ()
    {-# INLINE go #-}
{-# INLINE bracket #-}

-- | Lift a monad @m@ into a 'Factory'.
offer :: Monad m => m a -> Factory m env a
offer ma = wrap (ma >>=)
{-# INLINE offer #-}

-- | Put a delay action into 'Factory', it will run at close phase.
delay :: MonadMask m => m () -> Factory m env ()
delay ma = bracket (return ()) (const ma)
{-# INLINE delay #-}
