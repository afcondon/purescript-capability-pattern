module Rave where

import Prelude

import App.Layer.Four (Name(..))
import App.Layer.Three (class GetUserName, class Logger)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (class VariantShows, Variant, inj)
import Data.Variant.Internal (class VariantTags, RProxy(..))
import Effect.Aff (Aff, error)
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
import Rave.HTTP (class MonadHttp, HttpError, getEV)
import Rave.RowTypes (type (+))
import Record (get) as Record
import Type.Data.Row (RProxy)
import Type.Equality (class TypeEquals, from)

-- minimal scaffolding for call to getPureScript
mainEx = runRave (RProxy :: _ ()) { raveEnv: "dummy env" } do
  getPureScript

-- the unified function 
getPureScript
  ∷ ∀ m r
  . MonadHttp m
  ⇒ MonadFs m
  ⇒ ExceptV (HttpError + FsError + r) m Unit
getPureScript =
  getEV "http://purescript.org" >>= writeEV "~/purescript.html"


-- the type that we expect to be in the ReaderT as our environment
type Environment = { raveEnv :: String }

-- | Short for "Reader, Aff, Variant."
newtype Rave env var a = Rave (ReaderT env (ExceptV var Aff) a)

derive newtype instance raveMonadAff    :: MonadAff (Rave r v)
derive newtype instance raveMonadEffect :: MonadEffect (Rave r v)
derive newtype instance raveMonad       :: Monad (Rave r v)
derive newtype instance raveApplicative :: Applicative (Rave r v)
derive newtype instance raveApply       :: Apply (Rave r v)
derive newtype instance raveFunctor     :: Functor (Rave r v)
derive newtype instance raveBind        :: Bind (Rave r v)
derive newtype instance raveMonadError  :: MonadThrow (Variant v) (Rave r v)

-- | Capability instances
instance raveMonadHttp :: MonadHttp (Rave r v)
instance raveMonadFS   :: MonadFs (Rave r v)


instance raveMonadAsk :: TypeEquals e1 e2 => MonadAsk e1 (Rave e2 v) where
  ask = Rave $ asks from

instance loggerRave :: Logger (Rave r v) where
  log msg = liftEffect $ Console.log msg

instance getUserNameRave :: GetUserName Rave where
  getUserName = do
    env <- ask -- we still have access to underlying ReaderT

    result <- possiblyFailingCode env

    -- case result of
    --   Left (Error err) -> pure $ Name err
    --   Right res -> pure $ Name res
  
    pure $ Name "sort out the types first"

-- these are computations that can fail but the only requirement is at least Applicative 
-- failCode :: forall a. Applicative a => a (Either Error String)
failCode = liftAffV $ Left $ throw "A simple error"

-- successCode :: forall a. Applicative a => a (Either Error String)
successCode :: forall t17 t20. Applicative t17 => t17 (Either t20 String)
successCode = pure $ Right "Valid"

-- here we're using `do` so the requirement is Bind + Applicative = Monad
possiblyFailingCode :: forall m. Monad m => Environment -> m (Either Error String)
possiblyFailingCode _ = do
  x <- failCode
  y <- successCode
  pure x

-- original Rave machinery below

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

runRave :: forall v r rl a.
  RowToList v rl =>
  VariantTags rl =>
  VariantShows rl =>
  RProxy v ->
  r ->
  Rave r v a ->
  Aff a
runRave _ r (Rave rave) = do
  ran <- runExceptT $ runReaderT rave r
  case ran of
    Right res -> pure res
    Left l -> throwError $ error $ show l


-- itV :: forall r.
--   RProxy r
--   String ->
--   ExceptV r Aff Unit ->
--   Spec Unit
-- itV name toRun = it name $ runAffV RProxy toRun