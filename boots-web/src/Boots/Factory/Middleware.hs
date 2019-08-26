module Boots.Factory.Middleware where

import Network.Wai

type FactoryMiddleware env = env -> Application -> (env -> Application)