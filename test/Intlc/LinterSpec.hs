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
        lint (Message [Interpolation "x" (Select $ This (pure $ SelectCase "y" []))])
          `shouldBe` Success
        lint (Message [Interpolation "x" (Select $ These (pure $ SelectCase "y" []) (SelectWildcard []))])
          `shouldBe` Success

      it "fails on select with only a wildcard" $ do
        lint (Message [Interpolation "x" (Select $ That (SelectWildcard []))])
          `shouldBe` Failure (pure RedundantSelect)

  describe "internal" $ do
    describe "unicode" $ do
      let lint = lintWith' unsupportedUnicodeRule

      it "does not lint text with emoji" $ do
        lint (Message [Plaintext "Message with an emoji ❤️ 🥺"])
          `shouldBe` Failure (pure $ InvalidNonAsciiCharacter (fromList "❤️🥺"))

      it "does not lint text that is deeply nested with emoji" $ do
        lint (Message [Interpolation "Hello" (Callback []), Interpolation "Hello" (Bool [Plaintext "Message with an emoji 🥺"] [])])
          `shouldBe` Failure (fromList [InvalidNonAsciiCharacter (fromList ['🥺'])])

      it "lints streams without emoji" $ do
        lint (Message [Plaintext "Text without emoji"]) `shouldBe` Success

    describe "interpolations" $ do
      let lint = lintWith' interpolationsRule

      it "lints streams with 1 plain text token" $ do
        lint (Message [Plaintext "yay"]) `shouldBe` Success

      it "lints streams with 2 or more plain text token" $ do
        lint (Message [Plaintext "yay", Plaintext "Hello"]) `shouldBe` Success

      it "lints streams with 1 simple interpolation" $ do
        lint (Message [Interpolation "Hello" String]) `shouldBe` Success

      it "lints streams with 1 complex interpolation" $ do
        lint (Message [Interpolation "Hello" (Callback [])]) `shouldBe` Success

      it "lints streams with 1 complex interpolation and 1 simple interpolation" $ do
        lint (Message [Interpolation "Hello" (Callback []), Plaintext "hello"]) `shouldBe` Success

      it "does not lint streams with 2 or more complex interpolations" $ do
        lint (Message [Interpolation "Hello" (Callback []), Interpolation "Hello" (Bool [] [])])
          `shouldBe` Failure (pure TooManyInterpolations)

      it "does not lint nested streams" $ do
        lint (Message [Interpolation "outer" (Callback [Interpolation "inner" (Callback [])])])
          `shouldBe` Failure (pure TooManyInterpolations)

      it "does not lint complex interpolations with nested complex interpolations" $ do
        lint (Message [Interpolation "outer" (Select (This (pure $ SelectCase "hello" [Interpolation "super_inner" (Callback [])])))])
          `shouldBe` Failure (pure TooManyInterpolations)

      it "stops iterating after encountering two stream-interpolations" $ do
        let nested x = Interpolation "x" (Callback [x])
        let e = error "should not reach this item"

        lint (Message
          [ nested (nested e)
          , e
          ]) `shouldBe` Failure (pure TooManyInterpolations)
