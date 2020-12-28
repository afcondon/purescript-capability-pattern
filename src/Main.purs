module Main where

import Prelude

import App.Layer.Production (runApp, Environment) as S
import App.Layer.ProductionA (runApp, Environment) as A
import App.Layer.Test (runApp, Environment) as T
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

mainSync :: S.Environment -> Effect Unit
mainSync env = do
  result <- S.runApp program env
  pure unit

mainTest :: T.Environment -> Effect Unit
mainTest env = do
  assert $ (T.runApp program env) == "succeeds"
  assert $ (T.runApp program env) == "failing test"


mainAff :: A.Environment -> Effect Unit
mainAff env = launchAff_ do
  -- do aff-ish things here with ProductionA version
  result <- A.runApp program env
  -- now do some synchronous things within Aff using liftEffect
  result2 <- liftEffect $ S.runApp program env
  pure unit