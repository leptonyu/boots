{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
module Boots.Factory.Middleware.Random where

import           Boots
import           Boots.Factory.Web

buildRandom
  :: forall context env n
  . ( HasContextEntry context env
    , HasRandom env
    , MonadMask n
    , MonadIO n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildRandom pc pe = registerVault pc pe "Random" askRandom (lift . forkRD)
