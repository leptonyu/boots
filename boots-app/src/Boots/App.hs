module Boots.App(
    natA
  , delay
  , produceA
  , module Boots.App.Internal
  ) where

import           Boots.App.Internal
import           Control.Monad.Factory

natA :: MonadMask m => Factory (AppT env m) env component -> Factory m env component
natA fenc = do
  env <- getEnv
  natTrans (runAppT env) lift fenc
{-# INLINE natA #-}

delay :: MonadMask m => AppT env m () -> Factory m env ()
delay app = do
  env <- getEnv
  defer $ runAppT env app
{-# INLINE delay #-}

produceA :: MonadMask m => AppT env m res -> (res -> AppT env m ()) -> Factory m env res
produceA open close = do
  env <- getEnv
  produce (runAppT env open) (runAppT env . close)
{-# INLINE produceA #-}
