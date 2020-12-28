module Main where

import Prelude

import App.Layer.Production (runApp, Environment) as Sync
import App.Layer.ProductionA (runApp, Environment) as Async
import App.Layer.Test (runApp, Environment) as Test
import App.Layer.Three (program)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Assert (assert)


-- | Layer 0 Production
main :: Effect Unit
main = do
  mainSync { someState: "Sync"}
  mainAff  { someState: "Aff"}
  mainTest { someState: "Test"}
  pure unit

mainSync :: Sync.Environment -> Effect Unit
mainSync env = do
  result <- Sync.runApp program env
  pure unit

mainTest :: Test.Environment -> Effect Unit
mainTest env = do
  assert $ (Test.runApp program env) == "succeeds"
  assert $ (Test.runApp program env) == "failing test"


mainAff :: Async.Environment -> Effect Unit
mainAff env = launchAff_ do
  -- do aff-ish things here with ProductionA version
  result <- Async.runApp program env
  -- now do some synchronous things within Aff using liftEffect
  result2 <- liftEffect $ Sync.runApp program env
  pure unit