module Intlc.LinterSpec where

import           Data.These   (These (..))
import           Intlc.ICU
import           Intlc.Linter
import           Prelude
import           Test.Hspec

lintWith' :: Rule a -> Message -> Status a
lintWith' = lintWith . pure

spec :: Spec
spec = describe "linter" $ do
  describe "external" $ do
    describe "redundant select" $ do
      let lint = lintWith' redundantSelectRule

      it "succeeds on select with any non-wildcard case" $ do
        lint (Message [Select "x" $ This (pure $ SelectCase "y" [])])
          `shouldBe` Success
        lint (Message [Select "x" $ These (pure $ SelectCase "y" []) (SelectWildcard [])])
          `shouldBe` Success

      it "fails on selects with only a wildcard" $ do
        let s n = Select n . That . SelectWildcard

        lint (Message [s "x" [s "y" []], s "z" []])
          `shouldBe` Failure (pure $ RedundantSelect ("x" :| ["y", "z"]))

    describe "redundant plural" $ do
      let lint = lintWith' redundantPluralRule

      it "succeeds on exact cardinal plural" $ do
        lint (Message [CardinalExact "x" (pure (PluralExact "42", []))])
          `shouldBe` Success

      it "succeeds on ordinal plural with any non-wildcard case" $ do
        lint (Message [Ordinal "x" [(PluralExact "42", [])] [] []])
          `shouldBe` Success
        lint (Message [Ordinal "x" [] [(Two, [])] []])
          `shouldBe` Success

      it "succeeds on inexact cardinal plural with any non-wildcard case" $ do
        lint (Message [CardinalInexact "x" [(PluralExact "42", [])] [] []])
          `shouldBe` Success
        lint (Message [CardinalInexact "x" [] [(Two, [])] []])
          `shouldBe` Success

      it "fails on ordinal plural with only a wildcard" $ do
        lint (Message [Ordinal "x" [] [] []])
          `shouldBe` Failure (pure . RedundantPlural . pure $ "x")

      it "fails on inexact cardinal plural with only a wildcard" $ do
        lint (Message [CardinalInexact "x" [] [] []])
          `shouldBe` Failure (pure . RedundantPlural . pure $ "x")

  describe "internal" $ do
    describe "unicode" $ do
      let lint = lintWith' unsupportedUnicodeRule

      it "does not lint text with emoji" $ do
        lint (Message [Plaintext "Message with an emoji ❤️ 🥺"])
          `shouldBe` Failure (pure $ InvalidNonAsciiCharacter (fromList "❤️🥺"))

      it "does not lint text that is deeply nested with emoji" $ do
        lint (Message [Callback "Hello" [], Bool "Hello" [Plaintext "Message with an emoji 🥺"] []])
          `shouldBe` Failure (fromList [InvalidNonAsciiCharacter (fromList ['🥺'])])

      it "lints streams without emoji" $ do
        lint (Message [Plaintext "Text without emoji"]) `shouldBe` Success

    describe "interpolations" $ do
      let lint = lintWith' interpolationsRule
      -- An example interpolation that's affected by this lint rule.
      let f n = Select n . That . SelectWildcard

      it "lints streams with 1 plain text node" $ do
        lint (Message [Plaintext "yay"]) `shouldBe` Success

      it "lints streams with 2 or more plain text node" $ do
        lint (Message [Plaintext "yay", Plaintext "Hello"]) `shouldBe` Success

      it "lints streams with 1 simple interpolation" $ do
        lint (Message [String "Hello"]) `shouldBe` Success

      it "lints streams with 1 complex interpolation" $ do
        lint (Message [f "Hello" []]) `shouldBe` Success

      it "lints streams with 1 complex interpolation and 1 simple interpolation" $ do
        lint (Message [f "Hello" [], Plaintext "hello"]) `shouldBe` Success

      it "lints plurals and callbacks" $ do
        let cb = flip Callback mempty
        lint (Message [cb "x", cb "y"]) `shouldBe` Success

        let p n = Ordinal n [] (pure (Zero, [])) []
        lint (Message [p "x", p "y"]) `shouldBe` Success

      it "does not lint streams with 2 or more complex interpolations" $ do
        lint (Message [f "x" [], f "y" []])
          `shouldBe` Failure (pure $ TooManyInterpolations ("x" :| ["y"]))
        lint (Message [f "x" [], f "y" [], f "z" []])
          `shouldBe` Failure (pure $ TooManyInterpolations ("x" :| ["y", "z"]))

      it "does not lint nested streams" $ do
        lint (Message [f "outer" [f "inner" []]])
          `shouldBe` Failure (pure $ TooManyInterpolations ("outer" :| ["inner"]))
