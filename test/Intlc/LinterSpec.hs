module Intlc.LinterSpec where

import           Intlc.ICU
import           Intlc.Linter
import           Prelude
import           Test.Hspec

spec :: Spec
spec = describe "linter" $ do
  it "lints static text" $ do
    lint (Static "Hello world") `shouldBe` Success

