{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Boots.Endpoint.Swagger where

import qualified Data.Swagger   as S
import           Data.Text      (Text, pack)
import           Data.Version   (Version, showVersion)
import           Data.Word
import           Lens.Micro
import           Servant
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif

type EndpointSwagger = "endpoints" :> "swagger" :> Get '[JSON] S.Swagger

-- | Swagger modification
baseInfo
  :: String  -- ^ Hostname
  -> Text    -- ^ Server Name
  -> Version -- ^ Server version
  -> Word16  -- ^ Port
  -> S.Swagger -- ^ Old swagger
  -> S.Swagger
baseInfo hostName n v p s = s
  & S.info . S.title   .~ (n <> " API Documents")
  & S.info . S.version .~ pack (showVersion v)
  & S.host ?~ S.Host hostName (Just $ fromIntegral p)
