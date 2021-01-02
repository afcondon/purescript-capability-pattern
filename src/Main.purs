module Main where

import Prelude

import App.Layer.Production (runApp, Environment) as Sync
import App.Layer.ProductionA (runApp, Environment) as Async
import App.Layer.ProductionE (runApp, Environment) as Ex
import App.Layer.Test (runApp, Environment) as Test
import App.Layer.Three (program)
import App.Rave as Rave
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Test.Assert (assert)


-- | Layer 0 Production
main :: Effect Unit
-- main = mainExceptions { exceptEnv: "ExceptT" }
main = launchAff_ do
  result1 <- Async.runApp program { asyncEnv: "async.txt" }
  result2 <- Rave.runApp program { raveEnv: "Edgar Allen Poe"}
  liftEffect $ mainSync { productionEnv: "sync.txt" }
  liftEffect $ mainTest { testEnv: "Test" }
  pure unit


-- Three different "main" functions for three different scenarios
mainSync :: Sync.Environment -> Effect Unit
mainSync env = do
  result <- Sync.runApp program env
  pure unit

mainTest :: Test.Environment -> Effect Unit
mainTest env = do
  assert $ (Test.runApp program env) == "succeeds"

mainAff1 :: Async.Environment -> Effect Unit
mainAff1 env = launchAff_ do
  result <- Async.runApp program env
  pure unit

mainExceptions :: Ex.Environment -> Effect Unit
mainExceptions env = launchAff_ do
  result1 <- Ex.runApp program env
  result2 <- Ex.runApp program { exceptEnv: "" }
  pure unit

mainRave :: Rave.Environment -> Effect Unit
mainRave env = launchAff_ do
  result <- Rave.runApp program env
  pure unit

-- mainAff more complicated version able to call mainSync and mainTest
combinedMain :: Effect Unit
combinedMain = launchAff_ do
  -- we can do aff-ish things here with Async/ProductionA version
  result <- Async.runApp program { asyncEnv: "async.txt" }
  -- ...also able to do synchronous things (within Aff) using liftEffect
  liftEffect $ mainSync { productionEnv: "sync.txt" }
  liftEffect $ mainTest { testEnv: "Test" }
  pure unit