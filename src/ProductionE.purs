module App.Layer.ProductionE where
-- Layers One and Two have to be in same file due to orphan instance restriction

import Prelude

import App.Layer.Four (Name(..))
import App.Layer.Three (class Logger, class GetUserName)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except (Except, ExceptT(..), runExcept, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log) as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile) as Async
import Type.Equality (class TypeEquals, from)


newtype ErrorV = ErrorV String -- level up with Variant when this is type-checked and running
derive newtype instance showErrorV :: Show ErrorV

-- | Layer 2 Define our "Production" Monad but using Aff...
type Environment = { exceptEnv :: String }
newtype AppME a = AppME (ReaderT Environment (ExceptT ErrorV Aff) a)


-- | ...and the means to run computations in it
runApp :: forall a. AppME a -> Environment  -> Aff (Either ErrorV a)
runApp (AppME reader_T) env = runExceptT (runReaderT reader_T env) 

-- | Layer 1 Production in Aff
derive newtype instance functorAppME     :: Functor AppME
derive newtype instance applyAppME       :: Apply AppME
derive newtype instance applicativeAppME :: Applicative AppME
derive newtype instance bindAppME        :: Bind AppME
derive newtype instance monadAppME       :: Monad AppME
derive newtype instance monadEffectAppME :: MonadEffect AppME
derive newtype instance monadAffAppME    :: MonadAff AppME
derive newtype instance monadThrowAppME  :: MonadThrow ErrorV AppME
derive newtype instance monadErrorAppME  :: MonadError ErrorV AppME

-- | Reader instance not quite as simple a derivation as "derive newtype",
-- | as it needs TypeEquals for the env
instance monadAskAppME :: TypeEquals e Environment => MonadAsk e AppME where
  ask = AppME $ asks from

-- | implementing Logger here just to the console, but in real world you'd use
-- | the available Env to determine log levels, output destination, DB handles etc
-- | because this version runs in Aff you can do Aff-ish things here (not shown)
instance loggerAppME :: Logger AppME where
  log = liftEffect <<< Console.log

-- | a version of getUserName that reads the name from a file 
-- | given in the Environment
instance getUserNameAppME :: GetUserName AppME where
  getUserName = do
    env <- ask -- we still have access to underlying ReaderT

    liftEffect $ compute $ ExceptT (Identity (Right 5))

    -- after all this is done, we're still committed to returning a Name
    pure $ Name $ "didn't crash here!"

    
compute :: Except String Int -> Effect Unit
compute theComputation =
  case runExcept theComputation of
    Left error   -> Console.log $ "Failed computation! Error was:  " <> error
    Right output -> Console.log $ "Successful computation! Output: " <> show output
