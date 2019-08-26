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
  . ( HasLogger env
    , MonadMask n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildError _ _ = do
  registerMiddleware "ErrorReport" $ \webNT app req resH -> app req resH `catch`
    \e -> do
      unNT webNT (vault req) $ logException e
      resH (whenException e)
