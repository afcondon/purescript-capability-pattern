module App.Layer.Test where
-- Layers 1 & 2 must be in same module due to orphan instance restriction

import Prelude

import App.Layer.Four (Name(..))
import App.Layer.Three (class LogToScreen, class GetUserName)
import Control.Monad.Reader (Reader, runReader)

-- | Layer 2 Test
type Environment = { someState :: String }

newtype TestM a = TestM (Reader Environment a)
derive newtype instance functorTestM     :: Functor TestM
derive newtype instance applyTestM       :: Apply TestM
derive newtype instance applicativeTestM :: Applicative TestM
derive newtype instance bindTestM        :: Bind TestM
derive newtype instance monadTestM       :: Monad TestM

runTest :: forall a. TestM a -> Environment -> a
runTest (TestM reader) env = runReader reader env

-- | Layer 2 Test
instance logToScreenTestM :: LogToScreen TestM where
  log _ = pure unit -- no need to implement this

instance getUserNameTestM :: GetUserName TestM where
  getUserName = pure $ Name "error" -- replace with better test
