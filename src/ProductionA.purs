module App.Layer.ProductionA where
-- Layers One and Two have to be in same file due to orphan instance restriction

import Prelude

import App.Layer.Four (Name(..))
import App.Layer.Three (class LogToScreen, class GetUserName)
import Control.Monad.Reader (ReaderT, runReaderT)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log) as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- | Layer 2 Production
type Environment = { someState :: String }

newtype AppM a = AppM (ReaderT Environment Effect a)
derive newtype instance functorAppM     :: Functor AppM
derive newtype instance applyAppM       :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM        :: Bind AppM
derive newtype instance monadAppM       :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM

runApp :: forall a. AppM a -> Environment -> Aff a
runApp (AppM reader_T) env = 
  liftEffect $ runReaderT reader_T env


-- | Layer 1 Production
instance logToScreenAppM :: LogToScreen AppM where
  log = liftEffect <<< Console.log

instance getUserNameAppM :: GetUserName AppM where
  getUserName = liftEffect do
    contents <- readTextFile UTF8 "name.txt"
    pure $ Name contents
