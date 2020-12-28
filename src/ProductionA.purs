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

newtype AppMA a = AppMA (ReaderT Environment Aff a)
derive newtype instance functorAppMA     :: Functor AppMA
derive newtype instance applyAppMA       :: Apply AppMA
derive newtype instance applicativeAppMA :: Applicative AppMA
derive newtype instance bindAppMA        :: Bind AppMA
derive newtype instance monadAppMA       :: Monad AppMA
derive newtype instance monadEffectAppMA :: MonadEffect AppMA
derive newtype instance monadAffAppMA    :: MonadAff AppMA

runApp :: forall a. AppMA a -> Environment -> Aff a
runApp (AppMA reader_T) env = runReaderT reader_T env

-- | Layer 1 Production in Aff
instance logToScreenAppMA :: LogToScreen AppMA where
  log = liftEffect <<< Console.log

instance getUserNameAppMA :: GetUserName AppMA where
  getUserName = do -- running in AppMA
    -- when we read async or delay then we are trying to run in Aff
    
    -- which won't work as written because contents is Aff String
    -- contents <- Async.readTextFile UTF8 "name.txt"
    -- delay $ Milliseconds 1000.0 -- 1 second

    let contents = "Nemo" -- AppMA String
    pure $ Name $ contents
