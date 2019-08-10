module Boots.Factory.Application(
    HasApp(..)
  , AppEnv(..)
  , buildApp
  , rand64
  , buildRandom
  , getRand
  ) where

import           Boots.App.Internal
import           Boots.Factory
import           Boots.Factory.Logger
import           Boots.Factory.Salak
import           Boots.Factory.Vault
import           Control.Concurrent.MVar
import           Data.Maybe
import           Data.String
import           Data.Text               (Text)
import           Data.Version            (Version)
import           Data.Word
import           Lens.Micro
import           Lens.Micro.Extras
import           Numeric                 (showHex)
import           Salak
import           System.Random.SplitMix

class HasApp cxt env | env -> cxt where
  askApp :: Lens' env (AppEnv cxt)

instance HasApp cxt (AppEnv cxt) where
  askApp = id

instance HasSalak (AppEnv cxt) where
  askSourcePack = lens configure (\x y -> x {configure = y})

instance HasLogger (AppEnv cxt) where
  askLogger = lens logF (\x y -> x {logF = y})

instance HasVault cxt (AppEnv cxt) where
  askVault = lens vaultF (\x y -> x {vaultF = y})

data AppEnv cxt = AppEnv
  { name       :: Text    -- ^ Service name.
  , instanceId :: Text    -- ^ Instance id.
  , version    :: Version -- ^ Service version.
  , randSeed   :: MVar SMGen -- ^ Random seed
  , configure  :: Salak
  , logF       :: LogFunc
  , vaultF     :: VaultRef cxt
  }

buildApp :: (HasLogger cxt, MonadIO m, MonadCatch m) => String -> Version -> Factory m () (AppEnv cxt)
buildApp confName version = do
  configure  <- buildSalak confName
  within configure $ do
    name       <- fromMaybe (fromString confName) <$> require "application.name"
    randSeed   <- liftIO $ initSMGen >>= newMVar
    instanceId <- liftIO $ hex64 <$> random64 randSeed
    vaultF     <- liftIO $ newVaultRef
    logF       <- buildLogger vaultF (name <> "," <> instanceId)
    return AppEnv{..}

random64 :: MVar SMGen -> IO Word64
random64 ref = modifyMVar ref (return . go . nextWord64)
  where
    go (a,b) = (b,a)

hex64 :: IsString a => Word64 -> a
hex64 i = fromString $ let x = showHex i "" in replicate (16 - length x) '0' ++ x

rand64 :: (IsString a, MonadIO m) => MVar SMGen -> m a
rand64 = liftIO . fmap hex64 . random64

getRand :: (IsString a, HasApp cxt env, MonadIO m) => AppT env m a
getRand = do
  AppEnv{..} <- asks (view askApp)
  lift $ rand64 randSeed

buildRandom :: (IsString a, MonadIO m, HasApp cxt env) => Factory m env a
buildRandom = do
  AppEnv{..} <- asks (view askApp)
  offer $ rand64 randSeed
