module Main where

import Prelude

import App.Layer.Production (runApp)
import App.Layer.Test (runTest)
import App.Layer.Three (program)
import Effect (Effect)
import Test.Assert (assert)


-- | Layer 0 Production
main :: Effect Unit
main = do
  let globalEnvironmentInfo = { someState: "Main" }
  result <- runApp program globalEnvironmentInfo
  mainTest
  pure unit

mainTest :: Effect Unit
mainTest = do
  let globalEnvironmentInfo = { someState: "Test"}
  assert $ (runTest program globalEnvironmentInfo) == "replace with better test"