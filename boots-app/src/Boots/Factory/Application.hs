{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
module Boots.Factory.Application(
    HasApp(..)
  , AppEnv(..)
  , MonadRandom(..)
  , hex64
  , hex32
  , buildApp
  ) where

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
  askSalak = askPre . askSalak
  {-# INLINE askSalak #-}

instance HasVault cxt (AppEnv cxt) where
  askVault = askPre . askVault
  {-# INLINE askVault #-}

instance HasSalak (PreEnv cxt) where
  askSalak = lens configure (\x y -> x {configure = y})
  {-# INLINE askSalak #-}

instance HasVault cxt (PreEnv cxt) where
  askVault = lens vaultF (\x y -> x {vaultF = y})
  {-# INLINE askVault #-}

askPre :: Lens' (AppEnv cxt) (PreEnv cxt)
askPre = lens preEnv (\x y -> x { preEnv = y})

data AppEnv cxt = AppEnv
  { name       :: Text    -- ^ Service name.
  , instanceId :: Text    -- ^ Instance id.
  , version    :: Version -- ^ Service version.
  , logF       :: LogFunc
  , preEnv     :: PreEnv cxt
  }

data PreEnv cxt = PreEnv
  { vaultF    :: VaultRef cxt
  , configure :: Salak
  , randSeed  :: SMGen -- ^ Random seed
  }

class HasRandom env where
  askRandom :: Lens' env SMGen

instance HasRandom SMGen where
  askRandom = id
  {-# INLINE askRandom #-}

instance HasRandom (PreEnv env) where
  askRandom = lens randSeed (\x y -> x {randSeed = y})
  {-# INLINE askRandom #-}

instance HasRandom (AppEnv env) where
  askRandom = askPre . askRandom
  {-# INLINE askRandom #-}

class Monad m => MonadRandom env m | m -> env where
  nextW64 :: m Word64

instance HasRandom env => MonadRandom env (Factory n env) where
  nextW64 = do
    env <- get
    let (w, seed) = nextWord64 $ view askRandom env
    put $ over askRandom (const seed) env
    return w

buildApp :: forall cxt m. (HasLogger cxt, MonadIO m, MonadMask m) => String -> Version -> Factory m () (AppEnv cxt)
buildApp confName version = do
  mv <- liftIO $ newMVar []
  pe <- liftIO $ do
    randSeed  <- initSMGen
    configure <- runSalak def
      { configName = confName
      , loggerF = \c s -> modifyMVar_ mv $ return . ((c,s):)
      , loadExt = loadByExt YAML
      } askSourcePack
    return PreEnv{vaultF = VaultRef $ const id, ..}
  within pe $ do
    name       <- fromMaybe (fromString confName) <$> require "application.name"
    instanceId <- hex32 <$> nextW64
    logF       <- buildLogger @cxt (name <> "," <> instanceId)
    let lf c s = runLoggingT (logDebugCS c s :: LoggingT IO ()) (logfunc logF)
    liftIO $ swapMVar mv [] >>= sequence_ . reverse . fmap (uncurry lf)
    setLogF lf
    preEnv <- get
    return AppEnv{..}

hex64 :: IsString a => Word64 -> a
hex64 i = fromString $ let x = showHex i "" in replicate (16 - length x) '0' ++ x
{-# INLINE hex64 #-}

hex32 :: IsString a => Word64 -> a
hex32 i = fromString $ let x = showHex i "" in drop 8 $ replicate (16 - length x) '0' ++ x
{-# INLINE hex32 #-}

