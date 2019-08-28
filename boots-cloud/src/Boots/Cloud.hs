-- |
-- Module:      Boots.Cloud
-- Copyright:   2019 Daniel YU
-- License:     MIT
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- A quick out-of-box factory using to build microservices with many useful builtin components based on [boots-web](https://hackage.haskell.org/package/boots-web).
--
module Boots.Cloud(
  -- * Service management
    module Boots.Factory.Consul
  -- * Client
  , module Boots.Factory.Client
  ) where


import           Boots.Factory.Client
import           Boots.Factory.Consul
