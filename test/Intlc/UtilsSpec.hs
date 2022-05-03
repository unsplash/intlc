module Intlc.UtilsSpec (spec) where

import           Prelude
import           Test.Hspec
import           Utils      (dupes)

spec :: Spec
spec = describe "utils" $ do
  describe "dupes" $ do
    let f = dupes

    it "returns identity on empty input" $ do
      f [] `shouldBe` []

    it "returns duplicates only once each" $ do
      f "abbcddde" `shouldBe` "bd"
