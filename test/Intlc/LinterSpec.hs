module Intlc.LinterSpec where

import           Intlc.ICU
import           Intlc.Linter
import           Prelude
import           Test.Hspec

spec :: Spec
spec = describe "linter" $ do
  it "lints static text" $ do
    lint (Static "Hello world") `shouldBe` Success

  it "lints dynamic streams with 1 plain text token" $ do
    lint (Dynamic (fromList [Plaintext "yay"])) `shouldBe` Success

  it "lints dynamic streams with 2 or more plain text token" $ do
    lint (Dynamic (fromList [Plaintext "yay", Plaintext "Hello"])) `shouldBe` Success

  it "lints dynamic streams with 1 simple interpolation" $ do
    lint (Dynamic (fromList [Interpolation (Arg "Hello" String)])) `shouldBe` Success

  it "lints dynamic streams with 1 complex interpolation" $ do
    lint (Dynamic (fromList [Interpolation (Arg "Hello" (Callback []))])) `shouldBe` Success

  it "lints dynamic streams with 1 complex interpolation and 1 simple interpolation" $ do
    lint (Dynamic (fromList [Interpolation (Arg "Hello" (Callback [])), Plaintext "hello"])) `shouldBe` Success

  it "does not lint dynamic streams with 2 or more complex interpolations" $ do
    lint (Dynamic (fromList [Interpolation (Arg "Hello" (Callback [])), Interpolation (Arg "Hello" (Bool [] []))])) `shouldBe` Failure TooManyInterpolations

  it "does not lint nested dynamic streams" $ do
    lint (Dynamic (fromList [Interpolation (Arg "outer" (Callback [Interpolation (Arg "inner" (Callback []))]))])) `shouldBe` Failure TooManyInterpolations

  it "does not lint complex interpolations with nested complex interpolations" $ do
    lint (Dynamic (fromList [Interpolation (Arg "outer" (Select (fromList [SelectCase "hello" [Interpolation (Arg "super_inner" (Callback []))]]) Nothing))])) `shouldBe` Failure TooManyInterpolations

