module Boots(
    module Control.Monad.Factory
  , module Boots.App
  , module Boots.Random
  , module Boots.Factory.Salak
  , module Boots.Factory.Application
  , module Boots.Factory.Logger

  -- * Reexport
  , rightToMaybe
  , fromString
  , view
  , over
  , Lens'
  , lens
  , (&)
  , Proxy(..)
  , Default(..)
  ) where

import           Boots.App
import           Boots.Factory.Application
import           Boots.Factory.Logger
import           Boots.Factory.Salak
import           Boots.Random
import           Control.Monad.Factory

import           Data.Default
import           Data.Proxy
import           Data.String
import           Lens.Micro
import           Lens.Micro.Extras


rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right b) = Just b

