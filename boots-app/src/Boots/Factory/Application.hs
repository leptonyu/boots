module Boots.Factory.Application(
    HasApp(..)
  , AppEnv(..)
  , buildApp
  , rand64
  , buildRandom
  ) where

import           Boots.Factory
import           Boots.Factory.Logger
import           Boots.Factory.Salak
import           Control.Concurrent.MVar
import           Data.Maybe
import           Data.String             (fromString)
import           Data.Text               (Text)
import           Data.Version            (Version)
import           Data.Word
import           Lens.Micro
import           Lens.Micro.Extras
import           Numeric                 (showHex)
import           Salak
import           System.Random.SplitMix

class HasApp env where
  askApp :: Lens' env AppEnv

instance HasApp AppEnv where
  askApp = id

instance HasSalak AppEnv where
  askSourcePack = lens configure (\x y -> x {configure = y})

instance HasLogger AppEnv where
  askLogger = lens logF (\x y -> x {logF = y})

data AppEnv = AppEnv
  { name       :: Text    -- ^ Service name.
  , instanceId :: Text    -- ^ Instance id.
  , version    :: Version -- ^ Service version.
  , tags       :: [Text]  -- ^ Service tags.
  , randSeed   :: MVar SMGen -- ^ Random seed
  , configure  :: Salak
  , logF       :: LogFunc
  }

buildApp :: (MonadIO m, MonadCatch m) => String -> Version -> Factory m () AppEnv
buildApp confName version = do
  configure  <- buildSalak confName
  within configure $ do
    name       <- fromMaybe (fromString confName) <$> require "application.name"
    logF       <- buildLogger name
    tags       <- require "application.tags"
    randSeed   <- offer $ liftIO $ initSMGen >>= newMVar
    instanceId <- offer $ liftIO $ hex64 <$> random64 randSeed
    return AppEnv{..}

random64 :: MVar SMGen -> IO Word64
random64 ref = modifyMVar ref (return . go . nextWord64)
  where
    go (a,b) = (b,a)

hex64 :: Word64 -> Text
hex64 i = fromString $ let x = showHex i "" in replicate (16 - length x) '0' ++ x

rand64 :: MonadIO m => MVar SMGen -> m Text
rand64 = liftIO . fmap hex64 . random64

buildRandom :: (MonadIO m, HasApp env) => Factory m env Text
buildRandom = do
  AppEnv{..} <- asks (view askApp)
  offer $ rand64 randSeed
