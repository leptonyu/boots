{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
module Boots.Health(
    Health(..)
  , HealthStatus(..)
  , HasHealth(..)
  , emptyHealth
  , registerHealth
  ) where

import           Boots.Prelude
import           Control.Exception     (SomeException, catch)
import           Control.Monad.Factory
import qualified Data.HashMap.Strict   as HM
import           Data.Text             (Text, pack)
import           GHC.Generics

data HealthStatus = UP | DOWN deriving (Eq, Show, Generic)

data Health = Health
  { status  :: !HealthStatus
  , errMsg  :: !(Maybe Text)
  , details :: !(HM.HashMap Text Health)
  } deriving (Eq, Show, Generic)

{-# INLINE emptyHealth #-}
emptyHealth :: IO Health
emptyHealth = return (Health UP Nothing HM.empty)

class HasHealth env where
  askHealth :: Lens' env (IO Health)

instance HasHealth (IO Health) where
  askHealth = id

type CheckHealth = (Text, IO HealthStatus)

insertHealth :: CheckHealth -> IO Health -> IO Health
insertHealth (na, ios) ior = do
  (err,s)          <- ((Nothing,) <$> ios) `catch` (\(e :: SomeException) -> return (Just (pack $ show e), DOWN))
  Health{..} <- ior
  return (Health (if s == DOWN then s else status) Nothing $ HM.insert na (Health s err HM.empty) details)

-- combineHealth :: [CheckHealth] -> IO Health -> IO Health
-- combineHealth = flip (foldr insertHealth)

registerHealth
  :: (MonadMask n, HasHealth env)
  => Text
  -> IO HealthStatus
  -> Factory n env ()
registerHealth  name status = modifyEnv $ over askHealth $ insertHealth (name, status)
