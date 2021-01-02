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
import Test.Assert (assert)


-- | Layer 0 Production
main :: Effect Unit
main = launchAff_ do
  -- we can do aff-ish things here with Async/ProductionA version
  result1 <- Async.runApp program { asyncEnv: "async.txt" }
  result2 <- Rave.runApp program { raveEnv: "Edgar Allen Poe"}
  -- ...also able to do synchronous things (within Aff) using liftEffect
  liftEffect $ mainSync { productionEnv: "sync.txt" }
  liftEffect $ mainTest { testEnv: "Test" }
  pure unit


-- Different "main" functions for each of the different flavors of AppM
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
