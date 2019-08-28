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
  , HasWeb
  -- ** Configuration
  , HasWebConfig(..)
  , WebConfig(..)
  , EndpointConfig(..)
  -- ** Environment
  , WebEnv(..)
  , newWebEnv
  , askEnv
  -- ** Modified Middleware
  , EnvMiddleware
  , registerMiddleware
  -- ** Api serve
  , tryServe
  , trySwagger
  , tryServeWithSwagger
  -- ** Utilities
  , HasSwagger(..)
  , HasServer(..)
  , HasContextEntry(..)
  , SetContextEntry(..)
  , Context(..)
  , askContext
  , logException
  , whenException
  , ToSchema
  , Vault
  ) where

import           Boots
import           Boots.Endpoint.Swagger
import           Boots.Metrics
import           Control.Exception
    ( SomeException
    , fromException
    )
import qualified Data.HashMap.Strict                 as HM
import           Data.Maybe
import           Data.Swagger                        (Swagger)
import           Data.Swagger.Schema                 (ToSchema)
import           Data.Text                           (Text)
import           Data.Text.Lazy                      (toStrict)
import           Data.Text.Lazy.Encoding
import           Data.Word
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Salak
import           Servant
import           Servant.Server.Internal.ServerError (responseServerError)
import           Servant.Swagger

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

-- | Environment values with `WebConfig`.
class HasWebConfig env where
  askWebConfig :: Lens' env WebConfig

instance HasWebConfig WebConfig where
  askWebConfig = id

-- | Endpoint configuration.
data EndpointConfig = EndpointConfig
  { enabled   :: Bool
  , endpoints :: HM.HashMap Text Bool
  }

instance FromProp m EndpointConfig where
  fromProp = EndpointConfig
    <$> "enabled" .?= True
    <*> "enabled" .?= HM.empty

-- | Web environment, which defined all components to build a web application.
data WebEnv env context = WebEnv
  { serveW     :: forall api. HasServer api context
               => Proxy api -> Context context -> Server api -> Application
               -- ^ A wrapper of `serveWithContext`.
  , serveA     :: forall api. HasSwagger api
               => Proxy api -> Swagger
              -- ^ A wrapper of `toSwagger`.
  , middleware :: EnvMiddleware env -- ^ Modified middleware.
  , envs       :: env -- ^ Application environment.
  , context    :: env -> Context context -- ^ Function used to generate @context@ from @env.
  , config     :: WebConfig -- ^ Web configuration.
  , endpoint   :: EndpointConfig -- ^ Endpoint configuration.
  , store      :: Store -- ^ Metrics store.
  }

{-# INLINE askEnv' #-}
askEnv' :: Lens' (WebEnv env context) env
askEnv' = lens envs (\x y -> x { envs = y})

-- | Environment values with `WebEnv`.
instance HasWebConfig (WebEnv env context) where
  askWebConfig = lens config (\x y -> x { config = y})

instance HasMetrics (WebEnv env context) where
  {-# INLINE askMetrics #-}
  askMetrics = lens store (\x y -> x { store = y})
instance HasSalak env => HasSalak (WebEnv env context) where
  {-# INLINE askSalak #-}
  askSalak = askEnv' . askSalak
instance HasLogger env => HasLogger (WebEnv env context) where
  {-# INLINE askLogger #-}
  askLogger = askEnv' . askLogger
instance HasRandom env => HasRandom (WebEnv env context) where
  {-# INLINE askRandom #-}
  askRandom = askEnv' . askRandom
instance HasApp env => HasApp (WebEnv env context) where
  {-# INLINE askApp #-}
  askApp = askEnv' . askApp
instance HasHealth env => HasHealth (WebEnv env context) where
  {-# INLINE askHealth #-}
  askHealth = askEnv' . askHealth

-- | Unified constraints for web environment.
type HasWeb context env =
  ( HasApp env
  , HasLogger env
  , HasHealth env
  , HasRandom env
  , HasSalak env
  , HasContextEntry context env)

-- | Class type used to modify @context@ entries.
class HasContextEntry context env => SetContextEntry context env where
  setContextEntry :: env -> Context context -> Context context

instance {-# OVERLAPPABLE #-} SetContextEntry as env => SetContextEntry (a : as) env where
  {-# INLINE setContextEntry #-}
  setContextEntry env (a :. as) = a :. setContextEntry env as

instance SetContextEntry (env : as) env where
  {-# INLINE setContextEntry #-}
  setContextEntry env (_ :. as) = env :. as

-- | Lens for modify @env@ in @context@.
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

-- | Create a web environment.
{-# INLINE newWebEnv #-}
newWebEnv
  :: (HasContextEntry context env, HasLogger env)
  => env -- ^ Application environment.
  -> (env -> Context context) -- ^ Function used to generate @context@ from @env@.
  -> WebConfig -- ^ Web configuration.
  -> EndpointConfig -- ^ Endpoint configuration.
  -> Store -- ^ Metrics store.
  -> WebEnv env context
newWebEnv = WebEnv serveWithContext toSwagger id

-- | Get application environment @env@.
{-# INLINE askEnv #-}
askEnv :: MonadMask n => Factory n (WebEnv env context) env
askEnv = envs <$> getEnv

-- | Modified wai `Middleware`, which support modify @env@.
type EnvMiddleware env = (env -> Application) -> env -> Application

-- | Register a modified middleware.
{-# INLINE registerMiddleware #-}
registerMiddleware
  :: MonadMask n
  => EnvMiddleware env
  -> Factory n (WebEnv env context) ()
registerMiddleware md = modifyEnv $ \web -> web { middleware = md . middleware web }

-- | Build a web application from `WebEnv`.
buildWeb
  :: forall context env n
  . ( MonadIO n
    , MonadMask n
    , HasWeb context env
    )
  => Proxy context -- ^ @context@ proxy.
  -> Proxy env -- ^ @env@ proxy.
  -> Factory n (WebEnv env context) (IO ()) -- ^ Factory which create an application from `WebEnv`.
buildWeb _ _ = do
  (WebEnv{..} :: WebEnv env context) <- getEnv
  within envs $ do
    AppEnv{..}        <- asksEnv (view askApp)
    let serveWarp WebConfig{..} = runSettings
          $ defaultSettings
          & setPort (fromIntegral port)
          & setOnExceptionResponse whenException
          & setOnException (\_ -> runAppT envs . logException)
    let ok = enabled endpoint && HM.lookup "swagger" (endpoints endpoint) /= Just False
    when ok
      $ logInfo
      $ "Swagger enabled: http://"
      <> toLogStr (hostname config)
      <> ":"
      <> toLogStr (port config)
      <> "/endpoints/swagger"
    logInfo $ "Service started on port(s): " <> toLogStr (port config)
    delay $ logInfo "Service ended"
    return
      $ serveWarp config
      $ flip middleware envs
      $ \env1 -> if ok
        then serveW (Proxy @EndpointSwagger) (context env1)
              (return $ baseInfo (hostname config) name version (port config) $ serveA $ Proxy @EmptyAPI)
        else serveW (Proxy @EmptyAPI) (context env1) emptyServer

-- | Log exception.
{-# INLINE logException #-}
logException :: HasLogger env => SomeException -> App env ()
logException = logError . toLogStr . formatException

-- | Convert an exception into `Network.Wai.Response`.
{-# INLINE whenException #-}
whenException :: SomeException -> Network.Wai.Response
whenException e = responseServerError
  $ fromMaybe err400 { errBody = fromString $ show e} (fromException e :: Maybe ServerError)

-- | Format exception.
{-# INLINE formatException #-}
formatException :: SomeException -> Text
formatException e = case fromException e of
  Just ServerError{..} -> fromString errReasonPhrase <> " " <> toStrict (decodeUtf8 errBody)
  _                    -> fromString $ show e

-- | Serve web server with swagger.
tryServeWithSwagger
  :: forall env context api n
  . ( HasContextEntry context env
    , HasServer api context
    , HasSwagger api
    , MonadMask n)
  => Bool -- ^ If do this action.
  -> Proxy context -- ^ Context proxy.
  -> Proxy api -- ^ Api proxy.
  -> ServerT api (App env) -- ^ Api server.
  -> Factory n (WebEnv env context) ()
tryServeWithSwagger b pc proxy server = do
  trySwagger b    proxy
  tryServe   b pc proxy server

-- | Try serve a swagger definition.
trySwagger
  :: (MonadMask n, HasSwagger api)
  => Bool -- ^ If do this action.
  -> Proxy api -- ^ Api proxy.
  -> Factory n (WebEnv env context) ()
trySwagger b api = tryBuild b $ modifyEnv $ \web -> web { serveA = serveA web . gop api }

-- | Try serve a web server.
tryServe
  :: forall env context api n
  . ( HasContextEntry context env
    , HasServer api context
    , MonadMask n)
  => Bool -- ^ If do this action.
  -> Proxy context -- ^ Context proxy.
  -> Proxy api -- ^ Api proxy.
  -> ServerT api (App env) -- ^ Api server.
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


