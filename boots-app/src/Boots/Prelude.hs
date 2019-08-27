module Boots.Prelude(
  -- * Reexport
    rightToMaybe
  , whenJust
  , when
  , IsString(..)
  , view
  , over
  , Lens'
  , lens
  , (&)
  , Proxy(..)
  , Default(..)
  ) where

import           Control.Monad
import           Data.Default
import           Data.Proxy
import           Data.String
import           Lens.Micro
import           Lens.Micro.Extras

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right b) = Just b

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just a) f = f a
whenJust _        _ = pure ()
