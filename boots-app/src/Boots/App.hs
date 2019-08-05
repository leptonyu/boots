module Boots.App(
    natA
  , delayA
  , bracketA
  , module Boots.App.Internal
  ) where

import           Boots.App.Internal
import           Boots.Factory

natA :: Monad m => Factory (AppT env m) env component -> Factory m env component
natA fenc = do
  env <- ask
  natTrans (runAppT env) lift fenc

delayA :: MonadCatch m => AppT env m () -> Factory m env ()
delayA app = do
  env <- ask
  delay $ runAppT env app

bracketA :: MonadCatch m => AppT env m res -> (res -> AppT env m ()) -> Factory m env res
bracketA open close = do
  env <- ask
  bracket (runAppT env open) (runAppT env . close)
