{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Boots
import           Boots.Cloud
import           Boots.Web
import           Paths_demo  (version)
import           Servant

type API = "hello" :> Get '[JSON] NoContent

type AppE = (App (AppEnv ()))

apiServer :: ServerT API AppE
apiServer = logInfo "Hello" >> return NoContent

main = bootWebEnv "demo" Paths_demo.version (return ()) $ do
  buildConsul
  tryServeWithSwagger True (Proxy @'[AppEnv ()]) (Proxy @API) apiServer
