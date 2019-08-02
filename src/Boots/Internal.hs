-- |
-- Module:      Boots.Internal
-- Copyright:   2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Boot plugin and application.
--
module Boots.Internal(
  -- * Plugin
    module Boots.Internal.Plugin
  -- * Application
  , bootApp
  , delay
  , module Boots.Internal.App
  ) where

import           Boots.Internal.App
import           Boots.Internal.Plugin
import           Control.Monad.Catch

-- | Run application in context with the help of plugin. Context @cxt@ can't escape from @m@.
--  If you want to define your own `AppT` then please use 'boot' or 'runPlugin'.
bootApp :: Plugin () m cxt -> AppT cxt m () -> m ()
bootApp plugin app = runPlugin () plugin (`runAppT` app)

-- | Add a delayed action to plugin.
delay :: MonadCatch m => AppT env m () -> Plugin env m ()
delay app = ask >>= delayP . (`runAppT` app)
