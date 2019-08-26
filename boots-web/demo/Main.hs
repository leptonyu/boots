{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Boots
import           Boots.Web
import           Paths_boots_web (version)
import           Servant

type API = "hello" :> Get '[JSON] NoContent

apiServer :: ServerT API (App AppEnv)
apiServer = logInfo "Hello" >> return NoContent

main = boot $ do
  app  <- buildApp "demo" Paths_boots_web.version
  within app $ do
    conf <- require "application"
    logInfo $ "Start Service [" <> toLogStr (name app) <> "] ..."
    let
      c = newWebEnv
        (app :. EmptyContext)
         conf :: WebEnv AppEnv '[AppEnv]
      pe = Proxy @AppEnv
      pc = Proxy @'[AppEnv]
    within c $ do
      tryServe True  pc (Proxy @API) apiServer
      buildError     pc pe
      buildWebLogger pc pe
      buildTrace     pc pe
      buildRandom    pc pe
      buildWeb       pc pe
