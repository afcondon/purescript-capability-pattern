module App.Layer.Production where
-- Layers One and Two have to be in same file due to orphan instance restriction

import Prelude

import App.Layer.Four (Name(..))
import App.Layer.Three (class LogToScreen, class GetUserName)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log) as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile) as Sync
import Type.Equality (class TypeEquals, from)

-- | Layer 2 Production
type Environment = { someState :: String }

newtype AppM a = AppM (ReaderT Environment Effect a)
derive newtype instance functorAppM     :: Functor AppM
derive newtype instance applyAppM       :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM        :: Bind AppM
derive newtype instance monadAppM       :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM

-- not quite as simple a derivations, needs TypeEquals
instance monadAskAppM :: TypeEquals e Environment => MonadAsk e AppM where
  ask = AppM $ asks from

runApp :: forall a. AppM a -> Environment -> Effect a
runApp (AppM reader_T) env = runReaderT reader_T env


-- | Layer 1 Production
instance logToScreenAppM :: LogToScreen AppM where
  log = liftEffect <<< Console.log

instance getUserNameAppM :: GetUserName AppM where
  getUserName = do
    env <- ask
    Console.log env.someState
    contents <- liftEffect $ Sync.readTextFile UTF8 "sync.txt"
    pure $ Name contents
