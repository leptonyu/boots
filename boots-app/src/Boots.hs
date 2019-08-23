module Boots(
    module Control.Monad.Factory
  , module Boots.App
  , module Boots.Factory.Salak
  , module Boots.Factory.Application
  , module Boots.Factory.Logger
  , module Boots.Factory.Vault

  , demo
  ) where

import           Boots.App
import           Boots.Factory.Application
import           Boots.Factory.Logger
import           Boots.Factory.Salak
import           Boots.Factory.Vault
import           Control.Monad.Factory


import           Paths_boots_app           (version)
import           System.Environment


demo :: IO ()
demo = do
  setEnv "logging.level" "debug"
  running () (buildApp "demo" Paths_boots_app.version)
    $ \(e :: AppEnv LogFunc) -> runAppT e $ logInfo "hello"
