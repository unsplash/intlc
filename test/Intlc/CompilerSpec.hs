module Intlc.CompilerSpec (spec) where

import           Intlc.Compiler.Common
import           Intlc.ICU
import           Prelude
import           Test.Hspec

spec :: Spec
spec = describe "compiler" $ do
  describe "argument validation" $ do
    let numErrs = fromLeft 0 . first length . validateArgs

    it "validates against duplicates with incompatible types" $ do
      numErrs [Arg "x" Number, Arg "y" String, Arg "x" String, Arg "z" Number, Arg "y" String] `shouldBe` 1
      numErrs [Arg "x" Number, Arg "y" String, Arg "x" String, Arg "z" Number, Arg "z" String] `shouldBe` 2

    it "considers the underlying type when deducing incompatibility" $ do
      numErrs [Arg "x" Number, Arg "x" (Plural $ Cardinal (LitPlural (pure $ PluralCase (PluralExact "42") []) Nothing))] `shouldBe` 0
