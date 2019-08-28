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

apiServer :: ServerT API (App AppEnv)
apiServer = logInfo "Hello" >> return NoContent

main = bootWeb
  "demo"
  Paths_demo.version
  id
  (:. EmptyContext)
  (\_ _ -> buildConsul)
  (Proxy @API)
  apiServer
