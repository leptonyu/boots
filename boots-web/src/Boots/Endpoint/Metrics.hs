{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Boots.Endpoint.Metrics(
    HasMetrics(..)
  , endpointMetrics
  ) where

import           Boots
import           Boots.Endpoint.Class
import           Boots.Factory.Web
import           Boots.Metrics
import qualified Data.HashMap.Strict    as HM
import qualified Data.Map.Strict        as M
import           Data.Text              (Text)
import           Network.HTTP.Types
import           Network.Wai
import           Servant
import           System.Metrics
import qualified System.Metrics.Counter as Counter


type EndpointMetrics = "metrics" :> Get '[JSON] Metrics

type Metrics = M.Map Text Text

endpointMetrics
  ::( MonadMask n
    , MonadIO n
    , HasHealth env
    , HasLogger env
    , HasContextEntry context env)
  => Proxy context
  -> Factory n (WebEnv env context) ()
endpointMetrics pc = do
  store       <- asksEnv (view askMetrics)
  LogFunc{..} <- asksEnv (view askLogger)
  liftIO $ do
    registerGcMetrics store
    registerCounter "log.failure" logFail store
  let newC n = liftIO $ createCounter n store
  requests <- newC "http.server.requests"
  req_fail <- newC "http.server.requests.failure"
  registerMiddleware
      $ \app env req resH -> app env req
      $ \res -> do
        Counter.inc requests
        when (statusCode (responseStatus res) >= 400) $ Counter.inc req_fail
        resH res
  makeEndpoint "metrics" pc (Proxy @EndpointMetrics) (liftIO $ go store)
  where
    {-# INLINE go #-}
    go s = do
      sample <- sampleAll s
      return
        $ M.fromList
        $ HM.toList
        $ HM.map g2 sample
    {-# INLINE showT #-}
    showT :: Show a => a -> Text
    showT = fromString . show
    {-# INLINE g2 #-}
    g2 (Counter i)      = showT i
    g2 (Gauge i)        = showT i
    g2 (Label x)        = x
    g2 (Distribution i) = showT i
