module Jev.ClientSpec (spec) where

import Test.Hspec
import Jev.Client

spec :: Spec
spec = describe "Jev.Client" $ do
  it "retries on 429 and 5xx, not on 4xx (except 429) or 2xx" $ do
    map shouldRetry [200, 400, 404, 429, 500, 503]
      `shouldBe` [False, False, False, True, True, True]

  it "defaultConfig sets the documented base URL and model" $ do
    let c = defaultConfig "k"
    baseUrl c `shouldBe` "https://api.typesafe.ai"
    model c   `shouldBe` "jev-latest"
