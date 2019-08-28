{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module:      Boots.Factory.Random
-- Copyright:   2019 Daniel YU
-- License:     MIT
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module provide supports for creating a thread unsafe random seed.
--
module Boots.Factory.Random(
    buildRandom
  ) where

import           Boots
import           Boots.Factory.Web

-- | Create a thread unsafe random seed per request.
buildRandom
  :: forall context env n
  . (HasWeb context env, MonadMask n, MonadIO n)
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) ()
buildRandom _ _ = tryBuildByKey True "web.random.enabled" $
  registerMiddleware $ \app env req resH -> do
    seed <- unRD (view askRandom env) splitSMGen
    makeRD0 seed $ \rd -> app (over askRandom (const rd) env) req resH
