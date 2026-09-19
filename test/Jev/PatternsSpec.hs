module Jev.PatternsSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Jev.Types (ScoreAnswer(..))
import Jev.Patterns

spec :: Spec
spec = describe "Jev.Patterns" $ do
  it "band routes on thresholds (lo, hi)" $ do
    map (band (0.5, 0.8)) [0.9, 0.6, 0.2] `shouldBe` [Act, Confirm, Escalate]

  it "compositeScore normalises each score by (levels - 1) and weights" $ do
    -- two 3-level scores (max index 2): 1.0 -> 0.5, 2.0 -> 1.0; equal weights -> 0.75
    let sa s = ScoreAnswer { score = s
                           , legend = Map.fromList [(0,"a"),(1,"b"),(2,"c")]
                           , probabilities = Map.fromList [(0,0.0),(1,0.0),(2,0.0)]
                           , confidence = 1.0 }
    compositeScore [(1.0, sa 1.0), (1.0, sa 2.0)] `shouldBe` 0.75
