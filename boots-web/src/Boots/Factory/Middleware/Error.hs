{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
module Boots.Factory.Middleware.Error where

import           Boots
import           Boots.Factory.Web
import           Control.Exception (catch)
import           Network.Wai

{-# INLINE buildError #-}
buildError
  :: forall context env n
  . ( HasContextEntry context env
    , HasSalak env
    , HasLogger env
    , MonadIO n
    , MonadMask n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildError _ _ = tryBuildByKey True "web.error.enabled" $ do
  env <- askEnv
  registerMiddleware $ \app req resH -> app req resH `catch`
    \e -> do
      runVault env (vault req) $ logException e
      resH (whenException e)
