module App.Rave where

import Prelude

import App.Layer.Four (Name(..))
import App.Layer.Three (class GetUserName, class Logger)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Checked (ExceptV, handleError, safe)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Either (Either(..))
import Data.Variant (class VariantShows, Variant)
import Data.Variant.Internal (class VariantTags, RProxy(..))
import Effect.Aff (Aff, error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Node.Path (FilePath)
import Prim.RowList (class RowToList)
import Rave.FS (write) as FS
import Rave.HTTP (get) as HTTP
import Rave.FS (class MonadFs)
import Rave.HTTP (class MonadHttp)
import Type.Data.Row (RProxy)
import Type.Equality (class TypeEquals, from)

-- | ...and the means to run computations in it
runApp :: forall a. Rave Environment () a -> Environment -> Aff a
runApp rave env = runRave (RProxy :: _ ()) env rave
  where
    runRave :: forall var env rl.
      RowToList var rl =>
      VariantTags rl =>
      VariantShows rl =>
      RProxy var ->
      env ->
      Rave env var a ->
      Aff a
    runRave _ env (Rave rave) = do
      ran <- runExceptT $ runReaderT rave env
      case ran of
        Right res -> pure res
        Left l -> throwError $ error $ show l
    

-- the type that we expect to be in the ReaderT as our environment
type Environment = { raveEnv :: String }

-- | Short for "Reader, Aff, Variant."
newtype Rave env var a = Rave (ReaderT env (ExceptV var Aff) a)

derive newtype instance raveMonadAff    :: MonadAff (Rave env var)
derive newtype instance raveMonadEffect :: MonadEffect (Rave env var)
derive newtype instance raveMonad       :: Monad (Rave env var)
derive newtype instance raveApplicative :: Applicative (Rave env var)
derive newtype instance raveApply       :: Apply (Rave env var)
derive newtype instance raveFunctor     :: Functor (Rave env var)
derive newtype instance raveBind        :: Bind (Rave env var)
derive newtype instance raveMonadError  :: MonadThrow (Variant var) (Rave env var)

-- | Capability instances
instance raveMonadHttp :: MonadHttp (Rave env var)

instance raveMonadFS   :: MonadFs (Rave env var)

instance raveMonadAsk :: TypeEquals e1 e2 => MonadAsk e1 (Rave e2 v) where
  ask = Rave $ asks from

instance loggerRave :: Logger (Rave env var) where
  log msg = liftEffect $ Console.log msg

instance getUserNameRave :: GetUserName (Rave env var) where
  getUserName = do
    env <- ask -- we still have access to underlying ReaderT

    resultHttp <- safe $ HTTP.get "test" # handleError (errorHandlersBundleWithDefault "error")
    safe $ getPureScript # handleError (errorHandlersBundleWithDefault unit)
  
    pure $ Name resultHttp

-- the unified function 
-- getPureScript ∷ ∀ r env. Rave env (HttpError + FsError + r) Unit
getPureScript :: forall m t133
  . Monad m
  => MonadHttp m
  => MonadFs m
  => ExceptV ( fsFileNotFound     :: String
                , fsPermissionDenied :: Unit
                , httpNotFound    :: Unit
                , httpOther       :: { status :: Int, body :: String }
                , httpServerError :: String
                | t133
                ) m Unit
getPureScript = do
  HTTP.get "http://purescript.org" >>= FS.write "~/purescript.html"

errorHandlersBundleWithDefault :: forall m a.
  a -> 
  MonadEffect m =>
  { fsFileNotFound     :: FilePath -> m a
  , fsPermissionDenied :: Unit -> m a
  , httpNotFound       :: Unit -> m a
  , httpOther          :: { body :: String, status :: Int} -> m a
  , httpServerError    :: String -> m a
  }
errorHandlersBundleWithDefault defaultValue = {
    httpServerError:    \error -> do
      Console.log $ "Server error:" <> error
      pure defaultValue
  , httpNotFound:       \error -> do
      Console.log "Not found"
      pure defaultValue
  , httpOther:          \error -> do
      Console.log $ "Other: { status: " <> show error.status <> " , body: " <> error.body <> "}"
      pure defaultValue
  , fsFileNotFound:     \error -> do
      Console.log $ "File Not Found" <> error
      pure defaultValue
  , fsPermissionDenied: \error -> do
      Console.log "Permission Denied"
      pure defaultValue
}
