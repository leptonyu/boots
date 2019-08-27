module Boots.Metrics(
    HasMetrics(..)
  , Store
  , newStore
  ) where

import           Boots
import           System.Metrics

class HasMetrics env where
  askMetrics :: Lens' env Store
