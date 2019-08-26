{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
module Boots.Factory.Middleware.Random where

import           Boots
import           Boots.Factory.Web
import           Network.Wai

buildRandom
  :: forall context env n
  . ( HasContextEntry context env
    , HasRandom env
    , MonadMask n
    , MonadIO n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildRandom _ _ = do
  vr <- view askRandom <$> askEnv
  registerMiddleware $ \app req resH -> do
    seed <- forkRD vr
    app req { vault = modifyVault @env (over askRandom $ \_ -> seed) $ vault req } resH
