{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
-- |
-- Module:      Control.Monad.Factory.Class
-- Copyright:   2019 Daniel YU
-- License:     MIT
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
module Control.Monad.Factory.Class(
    MonadFactory(..)
  , defer
  , asksEnv
  , withEnv
  , modifyEnv
  , runEnv
  ) where

-- | Monads which allow to produce @component@ under @env@, and @env@ can be changed by this procedure.
class (Monad m, Monad n) => MonadFactory env n m | m -> env n where
  -- | Return the environment of the monad.
  getEnv  :: m env
  -- | Replace the environment inside the monad.
  putEnv  :: env -> m ()
  -- | Produce a resource component, with open and close.
  produce
    :: n component -- ^ Open resource
    -> (component -> n ()) -- ^ Close resource
    -> m component

-- | Asks sub value of env.
asksEnv :: MonadFactory env n m => (env -> a) -> m a
asksEnv f = f <$> getEnv

-- | Defer to run side effect when closeing resource.
defer :: MonadFactory env n m => n () -> m ()
defer = produce (return ()) . const

-- | Change environment @env@.
withEnv :: MonadFactory env n m => (env -> m env) -> m ()
withEnv f = getEnv >>= f >>= putEnv

-- | Modify environment @env@.
modifyEnv :: MonadFactory env n m => (env -> env) -> m ()
modifyEnv f = getEnv >>= putEnv . f

-- | Run factory, return component @c@ and updated environment @env@.
runEnv :: MonadFactory env n m => m c -> m (env, c)
runEnv ma = do
  a <- ma
  e <- getEnv
  return (e, a)
