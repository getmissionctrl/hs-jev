module Main (main) where

import Test.Hspec
import qualified Jev.TypesSpec
import qualified Jev.AskSpec
import qualified Jev.ClientSpec
import qualified Jev.PatternsSpec

main :: IO ()
main = hspec $ do
  Jev.TypesSpec.spec
  Jev.AskSpec.spec
  Jev.ClientSpec.spec
  Jev.PatternsSpec.spec
