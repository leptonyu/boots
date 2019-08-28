{-# LANGUAGE CPP        #-}
module Boots.Prelude(
  -- * Reexport
    rightToMaybe
  , whenJust
  , when
  , unless
  , IsString(..)
  , view
  , over
  , Lens'
  , lens
  , (&)
  , Monoid(..)
  , Proxy(..)
  , Default(..)
  ) where

import           Control.Monad
import           Data.Default
import           Data.Proxy
import           Data.String
import           Lens.Micro
import           Lens.Micro.Extras
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right b) = Just b

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just a) f = f a
whenJust _        _ = pure ()
