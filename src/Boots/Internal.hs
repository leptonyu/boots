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
  -- * Main function
    booting
  -- * Application
  , module Boots.Internal.App
  -- * Application plugin
  , module Boots.Internal.Plugin
  ) where

import           Boots.Internal.App
import           Boots.Internal.Plugin

-- | Run application using a plugin. Context @cxt@ can't escape from @m@.
booting :: Plugin () m cxt -> AppT cxt m () -> m ()
booting plugin app = runPlugin () plugin (`runAppT` app)
