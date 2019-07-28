-- |
-- Module:      Boots.Internal
-- Copyright:   2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Boot cores.
--
module Boots.Internal(
  -- * Plugin
    boot
  , module Boots.Internal.Plugin
  -- * Application
  , bootApp
  , module Boots.Internal.App
  ) where

import           Boots.Internal.App
import           Boots.Internal.Plugin

-- | Run application only in plugin.
boot :: Monad m => Plugin () m (m ()) -> m ()
boot plugin = runPlugin () plugin id

-- | Run application in context with the help of plugin. Context @cxt@ can't escape from @m@.
--  If you want to define your own `AppT` then please use 'boot' or 'runPlugin'.
bootApp :: Plugin () m cxt -> AppT cxt m () -> m ()
bootApp plugin app = runPlugin () plugin (`runAppT` app)
