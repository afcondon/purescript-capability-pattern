module App.Rave where

import Prelude

import App.Layer.Four (Name(..))
import App.Layer.Three (class GetUserName, class Logger)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Except.Checked (ExceptV, handleError, safe)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (class VariantShows, Variant, inj)
import Data.Variant.Internal (class VariantTags, RProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Node.FS.Aff (writeTextFile)
import Node.Path (FilePath)
import Prim.Row as R
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Rave.FS (class MonadFs, FsError, writeEV)
import Rave.HTTP (class MonadHttp, HttpError, getEV, getEVasUnit)
import Rave.RowTypes (type (+))
import Record (get) as Record
import Type.Data.Row (RProxy)
import Type.Equality (class TypeEquals, from)

-- | ...and the means to run computations in it
runApp :: forall a. Rave Environment () a -> Environment -> Aff a
runApp rave env = runRave (RProxy :: _ ()) env rave

-- the unified function 
-- getPureScript ∷ ∀ r env. Rave env (HttpError + FsError + r) Unit
getPureScript :: forall m t133
  . Monad m
  => MonadHttp m
  => MonadFs m
  => ExceptT (Variant
                ( fsFileNotFound     :: String
                , fsPermissionDenied :: Unit
                , httpNotFound    :: Unit
                , httpOther       :: { status :: Int, body :: String }
                , httpServerError :: String
                | t133
                )
              )
              m
              Unit
getPureScript = do
  getEV "http://purescript.org" >>= writeEV "~/purescript.html"


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

    resultHttp <- safe $ (getEVasUnit "test") # handleError {
          httpServerError:    \error -> Console.log $ "Server error:" <> error
        , httpNotFound:       \error -> Console.log "Not found"
        , httpOther:          \error -> Console.log $ "Other: { status: " <> show error.status <> " , body: " <> error.body <> "}"
        , fsFileNotFound:     \error -> Console.log $ "File Not Found" <> error
        , fsPermissionDenied: \error -> Console.log "Permission Denied"
      }
  
    pure $ Name "sort out the types first, then we'll see about names"

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



-- original Rave machinery below

class VariantInjTagged a b | a -> b where
  injTagged :: Record a -> Variant b

instance variantInjTagged ::
  ( RowToList r1 (RL.Cons sym a RL.Nil)
  , R.Cons sym a () r1
  , R.Cons sym a rx r2
  , IsSymbol sym
  ) =>
  VariantInjTagged r1 r2 where
    injTagged = inj (SProxy :: SProxy sym) <<< Record.get (SProxy :: SProxy sym)

throw :: forall m r1 r2 a.
  VariantInjTagged r1 r2 =>
  MonadThrow (Variant r2) m =>
  Record r1 ->
  m a
throw = throwError <<< injTagged

runRave :: forall var env rl a.
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

liftRave :: forall m a r. MonadError Error m => m a -> ExceptV (liftedError :: Error | r) m a
liftRave e = do
  run <- lift $ try e
  case run of
    Right r -> pure r
    Left l -> throw { liftedError: l }

liftAffV :: forall r m a. MonadAff m => Aff a -> ExceptV (liftedError :: Error | r) m a
liftAffV e = do
  run <- liftAff $ try e
  case run of
    Right r -> pure r
    Left l -> throw { liftedError: l }
 

-- itV :: forall r.
--   RProxy r
--   String ->
--   ExceptV r Aff Unit ->
--   Spec Unit
-- itV name toRun = it name $ runAffV RProxy toRun