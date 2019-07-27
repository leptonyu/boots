module Main where

import           Boots
import           Control.Concurrent
import           Control.Monad

main :: IO ()
main = booting (pluginSimple "application") go
  where
    go = forever $ do
      logInfo "Hello, world!"
      liftIO $ threadDelay 1000000
