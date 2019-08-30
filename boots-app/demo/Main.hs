{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Boots
import           Control.Monad
import           Data.Maybe
import           Data.Time
import           Paths_boots_app (version)

main :: IO ()
main = bootApp "demo" Paths_boots_app.version $ do
  env <- getEnv
  return $ runAppT env $ do
    count <- fromMaybe 1 <$> require "count"
    t0 <- liftIO getCurrentTime
    replicateM_ count $ logInfo "hello"
    t1 <- liftIO getCurrentTime
    liftIO $ print (diffUTCTime t1 t0)

