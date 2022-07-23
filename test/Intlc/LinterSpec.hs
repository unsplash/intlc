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

      it "fails on selects with only a wildcard" $ do
        let s = Select . That . SelectWildcard

        lint (Message [Interpolation "x" (s [Interpolation "y" (s [])]), Interpolation "z" (s [])])
          `shouldBe` Failure (pure $ RedundantSelect ("x" :| ["y", "z"]))

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
      -- An example interpolation that's affected by this lint rule.
      let f = Select . That . SelectWildcard

      it "lints streams with 1 plain text token" $ do
        lint (Message [Plaintext "yay"]) `shouldBe` Success

      it "lints streams with 2 or more plain text token" $ do
        lint (Message [Plaintext "yay", Plaintext "Hello"]) `shouldBe` Success

      it "lints streams with 1 simple interpolation" $ do
        lint (Message [Interpolation "Hello" String]) `shouldBe` Success

      it "lints streams with 1 complex interpolation" $ do
        lint (Message [Interpolation "Hello" (f [])]) `shouldBe` Success

      it "lints streams with 1 complex interpolation and 1 simple interpolation" $ do
        lint (Message [Interpolation "Hello" (f []), Plaintext "hello"]) `shouldBe` Success

      it "lints plurals and callbacks" $ do
        let cb = Callback []
        lint (Message [Interpolation "x" cb, Interpolation "y" cb]) `shouldBe` Success

        let p = Plural $ Ordinal [] (pure $ PluralCase Zero []) (PluralWildcard [])
        lint (Message [Interpolation "x" p, Interpolation "y" p]) `shouldBe` Success

      it "does not lint streams with 2 or more complex interpolations" $ do
        lint (Message [Interpolation "x" (f []), Interpolation "y" (f [])])
          `shouldBe` Failure (pure $ TooManyInterpolations ("x" :| ["y"]))
        lint (Message [Interpolation "x" (f []), Interpolation "y" (f []), Interpolation "z" (f [])])
          `shouldBe` Failure (pure $ TooManyInterpolations ("x" :| ["y", "z"]))

      it "does not lint nested streams" $ do
        lint (Message [Interpolation "outer" (f [Interpolation "inner" (f [])])])
          `shouldBe` Failure (pure $ TooManyInterpolations ("outer" :| ["inner"]))
