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
  , WebNT(..)
  , newWebEnv
  , registerMiddleware
  , registerVault
  , tryServe
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

data WebNT env = WebNT { unNT :: forall a. Vault -> App env a -> IO a }

data WebEnv env context = WebEnv
  { serveW     :: forall api. HasServer api context
               => WebNT env -> Proxy api -> Context context -> Server api -> Application
  , middleware :: WebNT env -> Middleware
  , webNT      :: WebNT env
  , context    :: Context context
  , config     :: WebConfig
  }

newWebEnv :: HasContextEntry context env => Context context -> WebConfig ->  WebEnv env context
newWebEnv c = WebEnv (const serveWithContext) (const id) (WebNT $ \_ -> runAppT (getContextEntry c)) c

registerMiddleware :: MonadMask n => String -> (WebNT env -> Middleware) -> Factory n (WebEnv env context) ()
registerMiddleware _ md = modifyEnv $ \web -> web { middleware = \e -> md e . middleware web e }

registerVault
  :: forall context env v n
  . ( HasContextEntry context env
    , MonadMask n
    , MonadIO n)
  => Proxy context -> Proxy env -> String -> Lens' env v -> (v -> App env v) -> Factory n (WebEnv env context) ()
registerVault _ _ name ls fv = do
  r <- view ls . getContextEntry . context <$> getEnv
  vr <- liftIO L.newKey
  let go v x = fromMaybe x $ L.lookup vr v
  modifyEnv $ \WebEnv{..} -> WebEnv
    { webNT = WebNT $ \v -> unNT webNT v . withAppT (over ls (go v))
    , ..}
  registerMiddleware name $ \webNT app req resH -> do
    r' <- unNT webNT (vault req) $ fv r
    app req { vault = L.insert vr r' $ vault req } resH

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
          & setOnException (\mreq -> unNT webNT (maybe L.empty vault mreq) . logException)
    logInfo $ "Service started on port(s): " <> portText
    delay $ logInfo "Service ended"
    return
      $ serveWarp config
      $ (middleware webNT)
      $ serveW webNT (Proxy @EmptyAPI) context emptyServer

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
  ::( HasServer api context
    , MonadMask n)
  => Bool
  -> Proxy context
  -> Proxy api
  -> ServerT api (App env)
  -> Factory n (WebEnv env context) ()
tryServe b pc proxy server
  = tryBuild b
  $ modifyEnv
  $ \web -> web { serveW = \w p c s -> serveW web w (gop p proxy) c
  $ s :<|> (\v -> hoistServerWithContext proxy pc (go . unNT w v) server) }
  where
    {-# INLINE go #-}
    go :: IO a -> Servant.Handler a
    go = liftIO
    {-# INLINE gop #-}
    gop :: forall a b. Proxy a -> Proxy b -> Proxy (a :<|> (Vault :> b))
    gop _ _ = Proxy


