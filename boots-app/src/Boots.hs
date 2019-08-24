{-# LANGUAGE OverloadedStrings #-}
module Boots(
    module Control.Monad.Factory
  , module Boots.App
  , module Boots.Factory.Salak
  , module Boots.Factory.Application
  , module Boots.Factory.Logger
  , module Boots.Factory.Vault

  , demoApp
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
import           Boots.Factory.Vault
import           Control.Monad.Factory

import           Data.Default
import           Data.Proxy
import           Data.String
import           Lens.Micro
import           Lens.Micro.Extras

import           Paths_boots_app           (version)
import           System.Environment


rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right b) = Just b

demoApp :: IO ()
demoApp = do
  setEnv "logging.level" "debug"
  running () (buildApp "demo" Paths_boots_app.version)
    $ \(e :: AppEnv LogFunc) -> runAppT e $ logInfo "hello"
