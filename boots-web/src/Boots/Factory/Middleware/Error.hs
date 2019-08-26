{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
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
    , HasLogger env
    , MonadMask n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildError _ _ = do
  env <- askEnv
  registerMiddleware $ \app req resH -> app req resH `catch`
    \e -> do
      runVault env (vault req) $ logException e
      resH (whenException e)
