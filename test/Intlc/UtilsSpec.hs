module Intlc.UtilsSpec (spec) where

import qualified Data.Map   as M
import           Prelude
import           Test.Hspec
import           Utils      (dupes, toNubMap)

spec :: Spec
spec = describe "utils" $ do
  describe "dupes" $ do
    let f = dupes

    it "returns identity on empty input" $ do
      f [] `shouldBe` []

    it "returns duplicates only once each" $ do
      f "abbcddde" `shouldBe` "bd"

  describe "toNubMap" $ do
    let f = toNubMap

    it "equivalent to M.fromList in the absence of duplicate keys" $ do
      let xs = [("a", 1), ("b", 2)] :: [(Text, Int)]
      f (error "absurd") xs `shouldBe` [M.fromList xs]

    it "calls input function in the presence of duplicate keys" $ do
      let onFail = pure . M.fromList . fmap (,42) . toList
      let xs = zip ["a", "b", "a", "c", "c", "d"] [1..] :: [(Text, Int)]
      let ys = zip ["a", "c"] (repeat 42)
      f onFail xs `shouldBe` [M.fromList ys]
