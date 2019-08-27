{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
module Boots.Middleware.Random where

import           Boots
import           Boots.Factory.Web

buildRandom
  :: forall context env n
  . ( HasContextEntry context env
    , HasRandom env
    , HasSalak env
    , MonadMask n
    , MonadIO n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildRandom _ _ = tryBuildByKey True "web.random.enabled" $
  registerMiddleware $ \app env req resH -> do
    seed <- unRD (view askRandom env) splitSMGen
    makeRD0 seed $ \rd -> app (over askRandom (const rd) env) req resH
