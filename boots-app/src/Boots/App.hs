module Boots.App(
    module Boots.App.Internal
  -- ** Utilities
  , natA
  , delay
  , produceA
  ) where

import           Boots.App.Internal
import           Control.Monad.Factory

-- | Nature transform from `AppT` @env@ @m@ to @m@.
natA :: MonadMask m => Factory (AppT env m) env component -> Factory m env component
natA fenc = do
  env <- getEnv
  natTrans (runAppT env) lift fenc
{-# INLINE natA #-}

-- | Add a delayed action into the factory.
delay :: MonadMask m => AppT env m () -> Factory m env ()
delay app = do
  env <- getEnv
  defer $ runAppT env app
{-# INLINE delay #-}

-- | Produce @res@ under environment @env@.
produceA :: MonadMask m => AppT env m res -> (res -> AppT env m ()) -> Factory m env res
produceA open close = do
  env <- getEnv
  produce (runAppT env open) (runAppT env . close)
{-# INLINE produceA #-}
