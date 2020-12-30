module App.Layer.ProductionE where
-- Layers One and Two have to be in same file due to orphan instance restriction

import Prelude

import App.Layer.Four (Name(..))
import App.Layer.Three (class Logger, class GetUserName)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, catchJust, try, withResource)
import Control.Monad.Except (Except, ExceptT(..), runExcept, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log) as Console
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

    -- this isn't running on our AppME monad, it's running on the given ExceptT Identity
    -- liftEffect do 
    --   compute' (try $ ExceptT ( Identity ( Right 5 ) ))
    --   compute' (try $ ExceptT ( Identity ( Left "an error occurred!" ) ))

    -- after all this is done, we're still committed to returning a Name
    pure $ Name $ "didn't crash here!"

compute' :: Except String (Either String Int) -> Effect Unit
compute' theComputation =
  case runExcept theComputation of
    Left error   -> Console.log $ "Failed computation! Error was:  " <> show error
    Right e_or_a -> case e_or_a of
      Left e  -> Console.log $ "Exposed error instance in do notation: "  <> show e
      Right a -> Console.log $ "Exposed output instance in do notation: " <> show a

{- 

compute :: forall e a. Show e => Show a => Except e a -> Effect Unit
compute theComputation =
  case runExcept theComputation of
    Left error   -> Console.log $ "Failed computation! Error was:  " <> show error
    Right output -> Console.log $ "Successful computation! Output: " <> show output

runMainFunction :: Effect Unit
runMainFunction = do
  Console.log "catchError:"
  compute (
    catchError
      (computationThatFailsWith "An error string")

      -- and a function that successfully handles the error
      (\errorString -> ExceptT (pure $ Right 5))
  )

  compute (
    catchError
      (computationThatFailsWith "An error string")

      -- and a function that cannot handle the error successfully
      (\errorString -> ExceptT (pure $ Left errorString))
  )

-------------------

data ErrorType
  = FailedCompletely
  | CanHandle TheseErrors

data TheseErrors
  = Error1
  | Error2

example_catchJust :: Effect Unit
example_catchJust = do
  Console.log "catchJust:"
  -- fail with an error that we ARE NOT catching...
  compute
    (catchJust
      ignore_FailedCompletely
      (computationThatFailsWith FailedCompletely)

      -- this function is never run because
      -- we ignore the "FailedCompletely" error instance
      handleError
    )

  -- fail with an error that we ARE catching...
  compute
    (catchJust
      ignore_FailedCompletely
      (computationThatFailsWith (CanHandle Error1))

      -- this function is run because we accept the
      -- error instance. It would also work if we threw `Error2`
      handleError
    )


ignore_FailedCompletely :: ErrorType -> Maybe TheseErrors
ignore_FailedCompletely FailedCompletely  = Nothing
ignore_FailedCompletely (CanHandle error) = Just error

handleError :: TheseErrors -> Except ErrorType Int
handleError Error1 = ExceptT (pure $ Right 5)
handleError Error2 = ExceptT (pure $ Right 6)

instance s1 :: Show ErrorType where
  show FailedCompletely  = "FailedCompletely"
  show (CanHandle error) = "CanHandle2 (" <> show error <> ")"

instance s2 :: Show TheseErrors where
  show Error1 = "Error1"
  show Error2 = "Error2"

-------------------

example_try :: Effect Unit
example_try = do
  Console.log "try: "
  compute' (try $ computationThatSucceedsWith 5)
  compute' (try $ computationThatFailsWith "an error occurred!")

-- In `try`, both the error and output isntance is returned,
-- thereby exposing it for usage in the do notation. To account for this,
-- we've modified `compute` slightly below.
-- Also, since we only specify either the error type or the output type above,
-- type inference can't figure out what the other type is. So,
-- it thinks that the unknown type doesn't have a "Show" instance
-- and the compilation fails.
-- Thus, we also specify both types below to avoid this problem.
compute' :: Except String (Either String Int) -> Effect Unit
compute' theComputation =
  case runExcept theComputation of
    Left error   -> Console.log $ "Failed computation! Error was:  " <> show error
    Right e_or_a -> case e_or_a of
      Left e  -> Console.log $ "Exposed error instance in do notation: "  <> show e
      Right a -> Console.log $ "Exposed output instance in do notation: " <> show a

-------------------

data Resource = Resource
instance showResource :: Show Resource where
  show x = "Resource"

example_withResource :: Effect Unit
example_withResource = do
  Console.log "withResource: "
  compute (
    withResource
      getResource
      cleanupResource
      computationThatUseResource
  )

getResource :: Except String Resource
getResource = computationThatSucceedsWith Resource

cleanupResource :: Resource -> Except String Unit
cleanupResource r =
  -- resource is cleaned up here
  -- and when finished, we return unit
  ExceptT (pure $ Right unit)

computationThatUseResource :: Resource -> Except String Int
computationThatUseResource r = -- do
  -- use resource here to compute some value
  ExceptT (pure $ Right 5)
 -}