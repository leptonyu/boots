{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
module Boots.Factory.Application(
    HasApp(..)
  , AppEnv(..)
  , MonadRandom(..)
  , HasRandom(..)
  , hex64
  , hex32
  , buildApp
  , RD(..)
  , makeIORefRD
  ) where

import           Boots.App.Internal
import           Boots.Factory.Logger
import           Boots.Factory.Salak
import           Boots.Factory.Vault
import           Control.Monad.Factory
import           Data.Default
import           Data.IORef
import           Data.Maybe
import           Data.String
import           Data.Text              (Text)
import           Data.Tuple
import           Data.Version           (Version)
import           Data.Word
import           Lens.Micro
import           Lens.Micro.Extras
import           Numeric                (showHex)
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
  , randSeed   :: RD -- ^ Random seed
  }

data RD = RD { unRD :: forall a. (SMGen -> (a, SMGen)) -> IO a }

class HasRandom env where
  askRandom :: Lens' env RD

instance HasRandom RD where
  askRandom = id
  {-# INLINE askRandom #-}

class Monad m => MonadRandom env m | m -> env where
  nextW64   :: m Word64
  nextSplit :: m RD

instance (HasRandom env, MonadMask n, MonadIO n) => MonadRandom env (Factory n env) where
  nextW64 = do
    rd <- asksEnv (view askRandom)
    liftIO $ unRD rd nextWord64
  nextSplit = do
    rd <- asksEnv (view askRandom)
    liftIO $ unRD rd splitSMGen >>= makeIORefRD

instance (HasRandom env, MonadIO n) => MonadRandom env (AppT env n) where
  nextW64 = do
    rd <- asks (view askRandom)
    liftIO $ unRD rd nextWord64
  nextSplit = do
    rd <- asks (view askRandom)
    liftIO $ unRD rd splitSMGen >>= makeIORefRD

{-# INLINE makeIORefRD #-}
makeIORefRD :: SMGen -> IO RD
makeIORefRD seed = newIORef seed >>= \ref -> return (RD $ \f -> atomicModifyIORef' ref (swap.f))

buildApp :: forall cxt m. (HasLogger cxt, MonadIO m, MonadMask m) => String -> Version -> Factory m () (AppEnv cxt)
buildApp confName version = do
  mv        <- liftIO $ newIORef []
  -- Initialize salak
  configure <- liftIO $ runSalak def
      { configName = confName
      , loggerF = \c s -> modifyIORef' mv ((c,s):)
      , loadExt = loadByExt YAML
      } askSourcePack
  -- Read application name
  name      <- within configure
    $ fromMaybe (fromString confName)
    <$> require "application.name"
  -- Generate instanceid
  randSeed    <- liftIO $ initSMGen >>= makeIORefRD
  instanceId  <- within randSeed $ hex32 <$> nextW64
  -- Initialize logger
  (vaultF, logF)         <- within (VaultRef $ const id)
    $ runEnv
    $ buildLogger configure (name <> "," <> instanceId)
  -- Consume logs from salak
  let lf c s = logCS c LevelTrace (toLogStr s) logF
  liftIO $ atomicModifyIORef' mv ([],) >>= sequence_ . reverse . fmap (uncurry lf)
  -- Config new logger to salak
  within configure $ setLogF lf
  return AppEnv{..}

hex64 :: IsString a => Word64 -> a
hex64 i = fromString $ let x = showHex i "" in replicate (16 - length x) '0' ++ x
{-# INLINE hex64 #-}

hex32 :: IsString a => Word64 -> a
hex32 i = fromString $ let x = showHex i "" in drop 8 $ replicate (16 - length x) '0' ++ x
{-# INLINE hex32 #-}

