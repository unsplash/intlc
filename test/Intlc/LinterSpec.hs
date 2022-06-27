module Intlc.LinterSpec where

import           Data.These   (These (..))
import           Intlc.ICU
import           Intlc.Linter
import           Prelude
import           Test.Hspec

spec :: Spec
spec = describe "linter" $ do
  describe "internal" $ do
    describe "interpolations" $ do
      it "lints streams with 1 plain text token" $ do
        lintInternal (Message [Plaintext "yay"]) `shouldBe` Success

      it "lints streams with 2 or more plain text token" $ do
        lintInternal (Message [Plaintext "yay", Plaintext "Hello"]) `shouldBe` Success

      it "lints streams with 1 simple interpolation" $ do
        lintInternal (Message [Interpolation "Hello" String]) `shouldBe` Success

      it "lints streams with 1 complex interpolation" $ do
        lintInternal (Message [Interpolation "Hello" (Callback [])]) `shouldBe` Success

      it "lints streams with 1 complex interpolation and 1 simple interpolation" $ do
        lintInternal (Message [Interpolation "Hello" (Callback []), Plaintext "hello"]) `shouldBe` Success

      it "does not lint streams with 2 or more complex interpolations" $ do
        lintInternal (Message [Interpolation "Hello" (Callback []), Interpolation "Hello" (Bool [] [])]) `shouldBe` Failure (pure TooManyInterpolations)

      it "does not lint nested streams" $ do
        lintInternal (Message [Interpolation "outer" (Callback [Interpolation "inner" (Callback [])])]) `shouldBe` Failure (pure TooManyInterpolations)

      it "does not lint complex interpolations with nested complex interpolations" $ do
        lintInternal (Message [Interpolation "outer" (Select (This (pure $ SelectCase "hello" [Interpolation "super_inner" (Callback [])])))]) `shouldBe` Failure (pure TooManyInterpolations)

      it "stops iterating after encountering two stream-interpolations" $ do
        let nested x = Interpolation "x" (Callback [x])
        let e = error "should not reach this item"

        lintInternal (Message
          [ nested (nested e)
          , e
          ]) `shouldBe` Failure (pure TooManyInterpolations)
