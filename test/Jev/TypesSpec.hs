module Jev.TypesSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (eitherDecode, Value)
import Jev.Types
import TestUtil (expectRight)

spec :: Spec
spec = describe "Jev.Types answer decoding" $ do
  it "decodes a Choice answer" $ do
    r <- eitherDecode <$> BL.readFile "test/fixtures/answer-choice.json"
    expectRight (r :: Either String ChoiceAnswer) $ \ca -> do
      ca.choice `shouldBe` "billing"
      Map.lookup "billing" ca.probabilities `shouldBe` Just 0.9
      ca.confidence `shouldBe` 0.82

  it "decodes a Score answer with integer-keyed maps" $ do
    r <- eitherDecode <$> BL.readFile "test/fixtures/answer-score.json"
    expectRight (r :: Either String ScoreAnswer) $ \sa -> do
      sa.score `shouldBe` 1.3
      Map.lookup 1 sa.probabilities `shouldBe` Just 0.7
      Map.lookup 2 sa.legend `shouldBe` Just "Blocking"

  it "parses a Noul to a bare Double via parseNoul" $ do
    r <- eitherDecode <$> BL.readFile "test/fixtures/answer-noul.json"
    expectRight (r :: Either String Value) $ \v ->
      expectRight (parseNoul v) (`shouldBe` 0.94)
