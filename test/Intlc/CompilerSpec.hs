module Intlc.CompilerSpec (spec) where

import           Intlc.Compiler.Common
import           Intlc.Core
import           Prelude
import           Test.Hspec

spec :: Spec
spec = describe "compiler" $ do
  describe "argument validation" $ do
    let numErrs = fromLeft 0 . first length . validateArgs

    it "removes duplicates" $ do
      validateArgs [Arg "x" Number, Arg "y" String, Arg "x" Number, Arg "z" Number] `shouldBe`
        Right [Arg "x" Number, Arg "y" String, Arg "z" Number]

    it "validates against duplicates with incompatible types" $ do
      numErrs [Arg "x" Number, Arg "y" String, Arg "x" String, Arg "z" Number] `shouldBe` 1
      numErrs [Arg "x" Number, Arg "y" String, Arg "x" String, Arg "z" Number, Arg "z" String] `shouldBe` 2

    it "considers the underlying type when deducing incompatibility" $ do
      numErrs [Arg "x" Number, Arg "x" (Plural (pure $ PluralCase "42" []) (PluralWildcard []))] `shouldBe` 0
