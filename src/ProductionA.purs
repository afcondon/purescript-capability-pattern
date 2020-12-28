module App.Layer.ProductionA where
-- Layers One and Two have to be in same file due to orphan instance restriction

import Prelude

import App.Layer.Four (Name(..))
import App.Layer.Three (class LogToScreen, class GetUserName)
import Control.Monad.Reader (ReaderT, runReaderT)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log) as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile) as Async

-- | Layer 2 Production in Aff
type Environment = { someState :: String }

newtype AppM a = AppM (ReaderT Environment Aff a)
derive newtype instance functorAppM     :: Functor AppM
derive newtype instance applyAppM       :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM        :: Bind AppM
derive newtype instance monadAppM       :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM    :: MonadAff AppM

runApp :: forall a. AppM a -> Environment -> Aff a
runApp (AppM reader_T) env = runReaderT reader_T env


-- | Layer 1 Production in Aff
instance logToScreenAppM :: LogToScreen AppM where
  log = liftEffect <<< Console.log

instance getUserNameAppM :: GetUserName AppM where
  getUserName = do -- running in AppM
    -- when we read async or delay then we are trying to run in Aff
    -- which won't work as written because contents is Aff String
    -- contents <- Async.readTextFile UTF8 "name.txt"
    -- delay $ Milliseconds 1000.0 -- 1 second
    let contents = "Nemo" -- AppM String
    pure $ Name $ contents
