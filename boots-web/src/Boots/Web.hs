module Boots.Web(
    module Boots.Factory.Web
  , module Boots.Factory.Middleware.Error
  , module Boots.Factory.Middleware.Logger
  , module Boots.Factory.Middleware.Random
  , module Boots.Factory.Middleware.Trace
  ) where
import           Boots.Factory.Middleware.Error
import           Boots.Factory.Middleware.Logger
import           Boots.Factory.Middleware.Random
import           Boots.Factory.Middleware.Trace
import           Boots.Factory.Web
