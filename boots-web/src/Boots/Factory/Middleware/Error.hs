{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
module Boots.Factory.Middleware.Error where

import           Boots
import           Boots.Factory.Web
import           Control.Exception (catch)

{-# INLINE buildError #-}
buildError
  :: forall context env n
  . ( HasContextEntry context env
    , HasSalak env
    , HasLogger env
    , MonadIO n
    , MonadMask n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildError _ _ = tryBuildByKey True "web.error.enabled" $
  registerMiddleware $ \app env req resH -> app env req resH `catch`
    \e -> do
      runAppT env $ logException e
      resH (whenException e)
