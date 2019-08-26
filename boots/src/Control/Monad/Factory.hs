{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-- |
-- Module:      Control.Monad.Factory
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
--
module Control.Monad.Factory(
  -- * Monad
    MonadFactory(..)
  , defer
  , asksEnv
  , modifyEnv
  , withEnv
  , runEnv
  -- * Monad Instance
  , Factory(..)
  -- ** Run functions
  , running
  , boot
  -- ** With
  , within
  , withFactory
  , wrap
  , liftFT
  , natTrans
  , tryBuild
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
  , MonadIO(..)
  , lift
  ) where

import qualified Control.Category            as C
import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Factory.Class
import           Control.Monad.State
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif


-- | Factory defines how to generate a @component@ under the environment @env@ in monad @m@.
-- It is similar to IoC container in oop, @env@ will provide anything to be wanted to generate @component@.
--
newtype Factory m env component
  = Factory { unFactory :: StateT env (ContT () m) component }
  deriving (Functor, Applicative, Monad, MonadState env, MonadIO)

instance MonadThrow m => MonadThrow (Factory m env) where
  {-# INLINE throwM #-}
  throwM = liftFT . throwM

instance Monad m => MonadCont (Factory m env) where
  {-# INLINE callCC #-}
  callCC a = do
    env <- get
    wrap . running env $ callCC a

instance C.Category (Factory m) where
  {-# INLINE id #-}
  id  = get
  {-# INLINE (.) #-}
  a . b = b >>= (`within` a)

instance MonadMask m => MonadFactory env m (Factory m env) where
  getEnv = get
  putEnv = put
  produce o = wrap . bracket o

-- | Running the factory.
running :: env -> Factory m env c -> (c -> m ()) -> m ()
running env pma = runContT (evalStateT (unFactory pma) env)
{-# INLINE running #-}

-- | Run the application using a specified factory.
boot :: Monad m => Factory m () (m ()) -> m ()
boot factory = running () factory id

-- | Construct factory under @env@, and adapt it to fit another @env'@.
within :: env -> Factory m env component -> Factory m env' component
within env = Factory . lift . (`evalStateT` env) . unFactory
{-# INLINE within #-}

-- | Construct factory under @env@, and adapt it to fit another @env'@.
withFactory :: (env' -> env) -> Factory m env component -> Factory m env' component
withFactory f (Factory ma) = do
  env <- get
  Factory (lift $ evalStateT ma (f env))
{-# INLINE withFactory #-}

-- | Wrap raw procedure into a 'Factory'.
wrap :: ((c -> m ()) -> m ()) -> Factory m env c
wrap = Factory . lift . ContT
{-# INLINE wrap #-}

-- | Lift a monad @m@ into a 'Factory'.
liftFT :: Monad m => m a -> Factory m env a
liftFT ma = wrap (ma >>=)
{-# INLINE liftFT #-}

-- | Nature transform of one 'Factory' with monad @n@ into another with monad @m@.
natTrans :: (n () -> m ()) -> (m () -> n ()) -> Factory n env component -> Factory m env component
natTrans fnm fmn fac = do
  env <- get
  wrap $ \fm -> fnm $ running env fac (fmn . fm)
{-# INLINE natTrans #-}

{-# INLINE tryBuild #-}
tryBuild :: Bool -> Factory n env () -> Factory n env ()
tryBuild b p = if b then p else return ()
