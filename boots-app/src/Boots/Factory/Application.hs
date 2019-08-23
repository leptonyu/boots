{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
module Boots.Factory.Application(
    HasApp(..)
  , AppEnv(..)
  , MonadRandom(..)
  , HasRandom(..)
  , hex64
  , hex32
  , buildApp
  ) where

import           Boots.Factory.Logger
import           Boots.Factory.Salak
import           Boots.Factory.Vault
import           Control.Concurrent.MVar
import           Control.Monad.Factory
import           Control.Monad.Logger.CallStack
import           Data.Default
import           Data.Maybe
import           Data.String
import           Data.Text                      (Text)
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

instance HasLogger (AppEnv cxt) where
  askLogger = lens logF (\x y -> x {logF = y})
  {-# INLINE askLogger #-}

instance HasSalak (AppEnv cxt) where
  askSalak = lens configure (\x y -> x {configure = y})
  {-# INLINE askSalak #-}

instance HasVault cxt (AppEnv cxt) where
  askVault = lens vaultF (\x y -> x {vaultF = y})
  {-# INLINE askVault #-}

instance HasRandom (AppEnv env) where
  askRandom = lens randSeed (\x y -> x {randSeed = y})
  {-# INLINE askRandom #-}

data AppEnv cxt = AppEnv
  { name       :: Text    -- ^ Service name.
  , instanceId :: Text    -- ^ Instance id.
  , version    :: Version -- ^ Service version.
  , logF       :: LogFunc
  , vaultF     :: VaultRef cxt
  , configure  :: Salak
  , randSeed   :: SMGen -- ^ Random seed
  }

class HasRandom env where
  askRandom :: Lens' env SMGen

instance HasRandom SMGen where
  askRandom = id
  {-# INLINE askRandom #-}

class Monad m => MonadRandom env m | m -> env where
  nextW64 :: m Word64

instance (HasRandom env, MonadMask n) => MonadRandom env (Factory n env) where
  nextW64 = do
    env <- getEnv
    let (w, seed) = nextWord64 $ view askRandom env
    putEnv $ over askRandom (const seed) env
    return w

buildApp :: forall cxt m. (HasLogger cxt, MonadIO m, MonadMask m) => String -> Version -> Factory m () (AppEnv cxt)
buildApp confName version = do
  mv        <- liftIO $ newMVar []
  -- Initialize salak
  configure <- liftIO $ runSalak def
      { configName = confName
      , loggerF = \c s -> modifyMVar_ mv $ return . ((c,s):)
      , loadExt = loadByExt YAML
      } askSourcePack
  -- Read application name
  name      <- within configure
    $ fromMaybe (fromString confName)
    <$> require "application.name"
  -- Generate instanceid
  (randSeed, instanceId) <- liftIO initSMGen >>> runEnv (hex32 <$> nextW64)
  -- Initialize logger
  (vaultF, logF)         <- within (VaultRef $ const id)
    $ runEnv
    $ buildLogger configure (name <> "," <> instanceId)
  -- Consume logs from salak
  let lf c s = runLoggingT (logDebugCS c s :: LoggingT IO ()) (logfunc logF)
  liftIO $ swapMVar mv [] >>= sequence_ . reverse . fmap (uncurry lf)
  -- Config new logger to salak
  within configure $ setLogF lf
  return AppEnv{..}

hex64 :: IsString a => Word64 -> a
hex64 i = fromString $ let x = showHex i "" in replicate (16 - length x) '0' ++ x
{-# INLINE hex64 #-}

hex32 :: IsString a => Word64 -> a
hex32 i = fromString $ let x = showHex i "" in drop 8 $ replicate (16 - length x) '0' ++ x
{-# INLINE hex32 #-}

