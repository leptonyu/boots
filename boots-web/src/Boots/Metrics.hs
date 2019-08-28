module Boots.Metrics(
    HasMetrics(..)
  , Store
  , newStore
  ) where

import           Boots
import           System.Metrics

-- | Environment values with `Store`.
class HasMetrics env where
  askMetrics :: Lens' env Store
