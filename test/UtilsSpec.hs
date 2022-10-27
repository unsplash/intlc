module UtilsSpec where

import           Prelude
import           Test.Hspec
import           Utils      (bun, bunBy)

spec :: Spec
spec = describe "Utils" $ do
  describe "bunBy" $ do
    it "returns duplicates from the left as per the predicate" $ do
      let xs = [("a" :: Text, 1 :: Int), ("b", 2), ("a", 3), ("c", 4), ("b", 5), ("a", 6)]
      let ys = [("a", 3), ("b", 5), ("a", 6)]

      bunBy ((==) `on` fst) xs `shouldBe` ys

    it "behaves equivalently for any foldable structure" $ do
      let xs = fromList [("a", 1), ("b", 2), ("a", 3), ("c", 4), ("b", 5), ("a", 6)] :: NonEmpty (Text, Int)
      let ys = [("a", 3), ("b", 5), ("a", 6)]

      bunBy ((==) `on` fst) xs `shouldBe` ys

  describe "bun" $ do
    it "returns duplicates from the left as per the Eq instance" $ do
      bun ["a" :: Text, "b", "a", "c", "b", "a"] `shouldBe`
        ["a", "b", "a"]

    it "behaves equivalently for any foldable structure" $ do
      bun (fromList ["a", "b", "a", "c", "b", "a"] :: NonEmpty Text) `shouldBe`
        ["a", "b", "a"]
