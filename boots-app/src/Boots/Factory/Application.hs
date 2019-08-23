{-# LANGUAGE ImplicitParams #-}
module Boots.Factory.Application(
    HasApp(..)
  , AppEnv(..)
  , buildApp
  , forkRand
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
import           Control.Monad.Logger.CallStack
import           Data.Default
import           Data.Maybe
import           Data.String
import           Data.Text                      (Text)
import           Data.Tuple
import           Data.Version                   (Version)
import           Data.Word
import           Lens.Micro
import           Lens.Micro.Extras
import           Numeric                        (showHex)
import           Salak
import           Salak.Yaml
import           System.Random.SplitMix

class HasApp cxt env | env -> cxt where
  askApp :: Lens' env (AppEnv cxt)

instance HasApp cxt (AppEnv cxt) where
  askApp = id
  {-# INLINE askApp #-}

instance HasSalak (AppEnv cxt) where
  askSalak = lens configure (\x y -> x {configure = y})
  {-# INLINE askSalak #-}

instance HasLogger (AppEnv cxt) where
  askLogger = lens logF (\x y -> x {logF = y})
  {-# INLINE askLogger #-}

instance HasVault cxt (AppEnv cxt) where
  askVault = lens vaultF (\x y -> x {vaultF = y})
  {-# INLINE askVault #-}

data AppEnv cxt = AppEnv
  { name       :: Text    -- ^ Service name.
  , instanceId :: Text    -- ^ Instance id.
  , version    :: Version -- ^ Service version.
  , randSeed   :: MVar SMGen -- ^ Random seed
  , configure  :: Salak
  , logF       :: LogFunc
  , vaultF     :: VaultRef cxt
  }

buildApp :: (HasLogger cxt, MonadIO m, MonadMask m) => String -> Version -> Factory m () (AppEnv cxt)
buildApp confName version = do
  mv           <- liftIO $ newMVar []
  configure    <- liftIO $ runSalak def
      { configName = confName
      , loggerF = \c s -> modifyMVar_ mv $ return . ((c,s):)
      , loadExt = loadByExt YAML
      } askSourcePack
  within configure $ do
    name       <- fromMaybe (fromString confName) <$> require "application.name"
    randSeed   <- liftIO $ initSMGen >>= newMVar
    instanceId <- liftIO $ hex32 <$> random64 randSeed
    vaultF     <- liftIO newVaultRef
    logF       <- buildLogger vaultF (name <> "," <> instanceId)
    let lf c s = runLoggingT (logDebugCS c s :: LoggingT IO ()) (logfunc logF)
    liftIO $ swapMVar mv [] >>= sequence_ . reverse . fmap (uncurry lf)
    setLogF lf
    return AppEnv{..}


forkRand :: MVar SMGen -> IO (MVar SMGen)
forkRand ms = do
  sv <- modifyMVar ms (return . splitSMGen)
  newMVar sv
{-# INLINE forkRand #-}

random64 :: MVar SMGen -> IO Word64
random64 ref = modifyMVar ref (return . swap . nextWord64)
{-# INLINE random64 #-}

hex64 :: IsString a => Word64 -> a
hex64 i = fromString $ let x = showHex i "" in replicate (16 - length x) '0' ++ x
{-# INLINE hex64 #-}

hex32 :: IsString a => Word64 -> a
hex32 i = fromString $ let x = showHex i "" in drop 8 $ replicate (16 - length x) '0' ++ x
{-# INLINE hex32 #-}

rand64 :: (IsString a, MonadIO m) => MVar SMGen -> m a
rand64 = liftIO . fmap hex64 . random64
{-# INLINE rand64 #-}

getRand :: (IsString a, HasApp cxt env, MonadIO m) => AppT env m a
getRand = do
  AppEnv{..} <- asks (view askApp)
  lift $ rand64 randSeed
{-# INLINE getRand #-}

buildRandom :: (IsString a, MonadIO m, HasApp cxt env) => Factory m env a
buildRandom = do
  AppEnv{..} <- asks (view askApp)
  offer $ rand64 randSeed
{-# INLINE buildRandom #-}
