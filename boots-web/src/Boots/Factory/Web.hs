{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Boots.Factory.Web(
    buildWeb
  , WebConfig(..)
  , WebEnv(..)
  , newWebEnv
  , registerMiddleware
  , askEnv
  , tryServe
  , runVault
  , modifyVault
  , HasContextEntry(..)
  , Context(..)
  , Vault
  , logException
  , whenException
  ) where

import           Boots
import           Control.Exception
    ( SomeException
    , fromException
    )
import           Data.Maybe
import           Data.Text                           (Text)
import           Data.Text.Lazy                      (toStrict)
import           Data.Text.Lazy.Encoding
import qualified Data.Vault.Lazy                     as L
import           Data.Word
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Salak
import           Servant
import           Servant.Server.Internal.ServerError (responseServerError)
import           System.IO.Unsafe                    (unsafePerformIO)
import           Unsafe.Coerce                       (unsafeCoerce)

-- | Application Configuration.
data WebConfig = WebConfig
  { hostname :: !String -- ^ Applicatoin hostname, used in swagger.
  , port     :: !Word16 -- ^ Application http port.
  } deriving (Eq, Show)

instance Default WebConfig where
  def = WebConfig "localhost" 8888

instance FromProp m WebConfig where
  fromProp = WebConfig
    <$> "host" .?: hostname
    <*> "port" .?: port

data WebEnv env context = WebEnv
  { serveW     :: forall api. HasServer api context
               => Proxy api -> Context context -> Server api -> Application
  , middleware :: Middleware
  , context    :: Context context
  , config     :: WebConfig
  }

newWebEnv
  :: (HasContextEntry context env, HasLogger env)
  => Context context -> WebConfig ->  WebEnv env context
newWebEnv = WebEnv serveWithContext id

{-# INLINE registerMiddleware #-}
registerMiddleware :: MonadMask n => Middleware -> Factory n (WebEnv env context) ()
registerMiddleware md = modifyEnv $ \web -> web { middleware = md . middleware web }


{-# INLINE askEnv #-}
askEnv :: (MonadMask n, HasContextEntry context env) => Factory n (WebEnv env context) env
askEnv = getContextEntry . context <$> getEnv

{-# NOINLINE keyEnv #-}
keyEnv :: L.Key ()
keyEnv = unsafePerformIO L.newKey

{-# INLINE runVault #-}
runVault :: env -> L.Vault -> App env a -> IO a
runVault env = runAppT . fromMaybe env . L.lookup (unsafeCoerce keyEnv)

{-# INLINE modifyVault #-}
modifyVault :: (env -> env) -> L.Vault -> L.Vault
modifyVault f v = flip L.adjust (unsafeCoerce keyEnv) f v

buildWeb
  :: forall context env n
  . ( MonadIO n
    , MonadMask n
    , HasApp env
    , HasLogger env
    , HasContextEntry context env
    )
  => Proxy context -> Proxy env -> Factory n (WebEnv env context) (IO ())
buildWeb _ _ = do
  (WebEnv{..} :: WebEnv env context) <- getEnv
  let fenv = getContextEntry context :: env
  within fenv $ do
    AppEnv{..}        <- asksEnv (view askApp)
    let portText = fromString (show $ port config)
        serveWarp WebConfig{..} = runSettings
          $ defaultSettings
          & setPort (fromIntegral port)
          & setOnExceptionResponse whenException
          & setOnException (\mreq -> runVault fenv (maybe L.empty vault mreq) . logException)
    logInfo $ "Service started on port(s): " <> portText
    delay $ logInfo "Service ended"
    return
      $ serveWarp config
      $ (\app req -> app req {vault = L.insert (unsafeCoerce keyEnv) fenv $ vault req})
      $ middleware
      $ serveW (Proxy @EmptyAPI) context emptyServer

{-# INLINE logException #-}
logException :: HasLogger env => SomeException -> App env ()
logException = logError . toLogStr . formatException

{-# INLINE whenException #-}
whenException :: SomeException -> Network.Wai.Response
whenException e = responseServerError
  $ fromMaybe err400 { errBody = fromString $ show e} (fromException e :: Maybe ServerError)

{-# INLINE formatException #-}
formatException :: SomeException -> Text
formatException e = case fromException e of
  Just ServerError{..} -> fromString errReasonPhrase <> " " <> toStrict (decodeUtf8 errBody)
  _                    -> fromString $ show e

tryServe
  ::( HasContextEntry context env
    , HasServer api context
    , MonadMask n)
  => Bool
  -> Proxy context
  -> Proxy api
  -> ServerT api (App env)
  -> Factory n (WebEnv env context) ()
tryServe b pc proxy server = tryBuild b $ do
  env <- askEnv
  modifyEnv
    $ \web -> web { serveW = \p c s -> serveW web (gop p proxy) c
    $ s :<|> (\v -> hoistServerWithContext proxy pc (go . runVault env v) server) }
  where
    {-# INLINE go #-}
    go :: IO a -> Servant.Handler a
    go = liftIO
    {-# INLINE gop #-}
    gop :: forall a b. Proxy a -> Proxy b -> Proxy (a :<|> (Vault :> b))
    gop _ _ = Proxy


