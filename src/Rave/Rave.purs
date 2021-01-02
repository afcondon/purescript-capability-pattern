module App.Rave where

import Prelude

import App.Layer.Four (Name(..))
import App.Layer.Three (class GetUserName, class Logger)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
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
import Rave.FS (class MonadFs, writeEV)
import Rave.HTTP (class MonadHttp, getEV)
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

    resultHttp <- safe $ getEV "test" # handleError errorHandlersBundle
    safe $ getPureScript # handleError errorHandlersBundle2
  
    pure $ Name resultHttp

{- 

-- these are computations that can fail but the only requirement is at least Applicative 
-- failCode :: forall a. Applicative a => a (Either Error String)
failCode = pure $ Left $ throw ?foo

-- successCode :: forall a. Applicative a => a (Either Error String)
-- successCode :: forall t17 t20. Applicative t17 => t17 (Either t20 String)
successCode = pure $ Right "Valid"

-- here we're using `do` so the requirement is Bind + Applicative = Monad
-- possiblyFailingCode :: forall m. Monad m => Environment -> m (Either Error String)
possiblyFailingCode _ = do
  x <- failCode
  y <- successCode
  pure x 
  
-}

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
  getEV "http://purescript.org" >>= writeEV "~/purescript.html"


errorHandlersBundle :: forall m.
  MonadEffect m =>
  { fsFileNotFound     :: FilePath -> m String
  , fsPermissionDenied :: Unit -> m String
  , httpNotFound       :: Unit -> m String
  , httpOther          :: { body :: String, status :: Int} -> m String
  , httpServerError    :: String -> m String
  }
errorHandlersBundle = {
    httpServerError:    \error -> do
      Console.log $ "Server error:" <> error
      pure "foo"
  , httpNotFound:       \error -> do
      Console.log "Not found"
      pure "foo"
  , httpOther:          \error -> do
      Console.log $ "Other: { status: " <> show error.status <> " , body: " <> error.body <> "}"
      pure "foo"
  , fsFileNotFound:     \error -> do
      Console.log $ "File Not Found" <> error
      pure "foo"
  , fsPermissionDenied: \error -> do
      Console.log "Permission Denied"
      pure "foo"
}

errorHandlersBundle2:: forall m.
  MonadEffect m =>
  { fsFileNotFound     :: FilePath -> m Unit
  , fsPermissionDenied :: Unit -> m Unit
  , httpNotFound       :: Unit -> m Unit
  , httpOther          :: { body :: String, status :: Int} -> m Unit
  , httpServerError    :: String -> m Unit
  }
errorHandlersBundle2 = {
    httpServerError:    \error -> do
      Console.log $ "Server error:" <> error
      pure unit
  , httpNotFound:       \error -> do
      Console.log "Not found"
      pure unit
  , httpOther:          \error -> do
      Console.log $ "Other: { status: " <> show error.status <> " , body: " <> error.body <> "}"
      pure unit
  , fsFileNotFound:     \error -> do
      Console.log $ "File Not Found" <> error
      pure unit
  , fsPermissionDenied: \error -> do
      Console.log "Permission Denied"
      pure unit
}

