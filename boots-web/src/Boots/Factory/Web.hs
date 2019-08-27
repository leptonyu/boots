{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Boots.Factory.Web(
    buildWeb
  , WebConfig(..)
  , WebEnv(..)
  , EnvMiddleware
  , newWebEnv
  , registerMiddleware
  , askEnv
  , tryServe
  -- , runVault
  -- , modifyVault
  , HasContextEntry(..)
  , SetContextEntry(..)
  , askContext
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
  {-# INLINE def #-}
  def = WebConfig "localhost" 8888

instance FromProp m WebConfig where
  {-# INLINE fromProp #-}
  fromProp = WebConfig
    <$> "host" .?: hostname
    <*> "port" .?: port

type EnvMiddleware env = (env -> Application) -> env -> Application

data WebEnv env context = WebEnv
  { serveW     :: forall api. HasServer api context
               => Proxy api -> Context context -> Server api -> Application
  , middleware :: EnvMiddleware env
  , envs       :: env
  , context    :: env -> Context context
  , config     :: WebConfig
  }

{-# INLINE askEnv' #-}
askEnv' :: Lens' (WebEnv env context) env
askEnv' = lens envs (\x y -> x { envs = y})

instance HasSalak env => HasSalak (WebEnv env context) where
  {-# INLINE askSalak #-}
  askSalak = askEnv' . askSalak
instance HasLogger env => HasLogger (WebEnv env context) where
  {-# INLINE askLogger #-}
  askLogger = askEnv' . askLogger
instance HasRandom env => HasRandom (WebEnv env context) where
  {-# INLINE askRandom #-}
  askRandom = askEnv' . askRandom

class HasContextEntry context env => SetContextEntry context env where
  setContextEntry :: env -> Context context -> Context context

instance {-# OVERLAPPABLE #-} SetContextEntry as env => SetContextEntry (a : as) env where
  {-# INLINE setContextEntry #-}
  setContextEntry env (a :. as) = a :. setContextEntry env as

instance SetContextEntry (env : as) env where
  {-# INLINE setContextEntry #-}
  setContextEntry env (_ :. as) = env :. as


{-# INLINE askContext #-}
askContext :: SetContextEntry context env => Lens' (Context context) env
askContext = lens getContextEntry (flip setContextEntry)

instance (SetContextEntry context env, HasSalak env) => HasSalak (Context context) where
  {-# INLINE askSalak #-}
  askSalak = askContext @context @env . askSalak
instance (SetContextEntry context env, HasLogger env) => HasLogger (Context context) where
  {-# INLINE askLogger #-}
  askLogger = askContext @context @env . askLogger
instance (SetContextEntry context env, HasRandom env) => HasRandom (Context context) where
  {-# INLINE askRandom #-}
  askRandom = askContext @context @env . askRandom

{-# INLINE newWebEnv #-}
newWebEnv
  :: (HasContextEntry context env, HasLogger env)
  => env -> (env -> Context context) -> WebConfig ->  WebEnv env context
newWebEnv = WebEnv serveWithContext id

{-# INLINE registerMiddleware #-}
registerMiddleware :: MonadMask n => EnvMiddleware env -> Factory n (WebEnv env context) ()
registerMiddleware md = modifyEnv $ \web -> web { middleware = md . middleware web }

{-# INLINE askEnv #-}
askEnv :: MonadMask n => Factory n (WebEnv env context) env
askEnv = envs <$> getEnv

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
  within envs $ do
    AppEnv{..}        <- asksEnv (view askApp)
    let portText = fromString (show $ port config)
        serveWarp WebConfig{..} = runSettings
          $ defaultSettings
          & setPort (fromIntegral port)
          & setOnExceptionResponse whenException
          & setOnException (\_ -> runAppT envs . logException)
    logInfo $ "Service started on port(s): " <> portText
    delay $ logInfo "Service ended"
    return
      $ serveWarp config
      $ flip middleware envs
      $ \env1 -> serveW (Proxy @EmptyAPI) (context env1) emptyServer

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
  :: forall env context api n
  . ( HasContextEntry context env
    , HasServer api context
    , MonadMask n)
  => Bool
  -> Proxy context
  -> Proxy api
  -> ServerT api (App env)
  -> Factory n (WebEnv env context) ()
tryServe b pc proxy server = tryBuild b $
  modifyEnv
    $ \web -> web { serveW = \p c s -> serveW web (gop p proxy) c
    $ s :<|> hoistServerWithContext proxy pc (go . runAppT (getContextEntry c :: env)) server }
  where
    {-# INLINE go #-}
    go :: IO a -> Servant.Handler a
    go = liftIO
    {-# INLINE gop #-}
    gop :: forall a b. Proxy a -> Proxy b -> Proxy (a :<|> b)
    gop _ _ = Proxy


