module Jev.AskSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson (Value(..), eitherDecode, object, (.=))
import Data.Either (isLeft)
import Data.List (sort)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Jev.Types
import Jev.Ask
import TestUtil (expectRight)

data Team = Billing | Returns deriving (Eq, Ord, Show, Enum, Bounded)

spec :: Spec
spec = describe "Jev.Ask" $ do
  it "encodes a two-question batch under positional keys q0, q1" $ do
    let ask = (,) <$> askChoice (plain "which team?")
                                [("billing", descPlain "charges"), ("returns", descNone)]
                  <*> askNoul   (plain "escalate?")
        qs  = encodeQuestions ask
    KM.keys qs `shouldMatchList` ["q0", "q1"]
    KM.lookup "q0" qs `shouldBe` Just
      (object [ "type" .= ("choice" :: String)
              , "instructions" .= ("which team?" :: String)
              , "criteria" .= object [ "billing" .= ("charges" :: String)
                                     , "returns" .= Null ] ])
    KM.lookup "q1" qs `shouldBe` Just
      (object [ "type" .= ("noul" :: String)
              , "instructions" .= ("escalate?" :: String) ])

  it "decodes a batch response into a typed tuple" $ do
    let ask = (,) <$> askChoice (plain "team?") [("billing", descNone), ("returns", descNone)]
                  <*> askNoul   (plain "escalate?")
    r <- eitherDecode <$> BL.readFile "test/fixtures/answers-batch.json"
    expectRight (r :: Either String (KM.KeyMap Value)) $ \am ->
      expectRight (decodeAnswers ask am) $ \(ca, n) -> do
        ca.choice `shouldBe` "billing"
        n `shouldBe` 0.7

  it "encode unions disjoint positional keys under <*>" $ do
    let f = askNoul (plain "a")
        g = (,) <$> askNoul (plain "b") <*> askNoul (plain "c")
        keysOf = KM.keys . encodeQuestions
    keysOf ((,) <$> f <*> g) `shouldMatchList` ["q0", "q1", "q2"]

  it "decode-independence: a sub-answer is invariant to added siblings" $ do
    let mkAsk extra = (\c _ -> c)
                        <$> askChoice (plain "team?") [("billing", descNone), ("returns", descNone)]
                        <*> extra
        answers = KM.fromList
          [ ("q0", Object (KM.fromList [ ("type", "choice"), ("choice", "billing")
              , ("probabilities", Object (KM.fromList [("billing", Number 1.0)]))
              , ("confidence", Number 1.0) ]))
          , ("q1", Object (KM.fromList [("type","noul"), ("noul", Number 0.5)])) ]
        r1 = decodeAnswers (mkAsk (askNoul (plain "x"))) answers
        r2 = decodeAnswers (mkAsk (askNoul (plain "y"))) answers
    expectRight r1 $ \c1 -> expectRight r2 $ \c2 -> c1.choice `shouldBe` c2.choice

  it "enum roundtrip: dist keys are enum members and chosen parses back" $ do
    let ask = askChoiceEnum (plain "team?") (const descNone) :: Ask (ChoiceResult Team)
        answers = KM.fromList
          [ ("q0", Object (KM.fromList [ ("type","choice"), ("choice","Billing")
              , ("probabilities", Object (KM.fromList [("Billing", Number 0.7), ("Returns", Number 0.3)]))
              , ("confidence", Number 0.6) ])) ]
    expectRight (decodeAnswers ask answers) $ \cr -> do
      cr.chosen `shouldBe` Billing
      Map.keys cr.dist `shouldMatchList` [Billing, Returns]

  it "enum decode fails on an unknown option key" $ do
    let ask = askChoiceEnum (plain "team?") (const descNone) :: Ask (ChoiceResult Team)
        answers = KM.fromList
          [ ("q0", Object (KM.fromList [ ("type","choice"), ("choice","Nope")
              , ("probabilities", Object (KM.fromList [("Nope", Number 1.0)]))
              , ("confidence", Number 1.0) ])) ]
    decodeAnswers ask answers `shouldSatisfy` isLeft

  it "prop: encode key count equals question count (fan-out is <*>)" $
    property $ \(NonNegative a) (NonNegative b) ->
      let n  = min a 8 :: Int
          m  = min b 8 :: Int
          qA = foldr (\_ acc -> (:) <$> askNoul (plain "x") <*> acc) (pure []) [1..n]
          qB = foldr (\_ acc -> (:) <$> askNoul (plain "y") <*> acc) (pure []) [1..m]
          ask = (,) <$> qA <*> qB
      in KM.size (encodeQuestions ask) == n + m

  it "prop: keys are exactly q0..q(k-1) with no gaps" $
    property $ \(NonNegative a) ->
      let n   = min a 12 :: Int
          ask = foldr (\_ acc -> (:) <$> askNoul (plain "x") <*> acc) (pure []) [1..n]
          ks  = map Key.toString (KM.keys (encodeQuestions ask))
      in sort ks == sort [ 'q' : show i | i <- [0 .. n-1] ]
