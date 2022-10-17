{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

module Intlc.ICUSpec where

import           Intlc.ICU
import           Prelude
import           Test.Hspec

spec :: Spec
spec = describe "ICU AST" $ do
  let a = Char' 'a'
  let b = Char' 'b'
  let c = Char' 'c'

  describe "semigroup" $ do
    describe "is lawful with respect to" $ do
      it "associativity" $ do
        (a <> b) <> c `shouldBe` a <> (b <> c)

  describe "monoid" $ do
    describe "is lawful with respect to" $ do
      it "left identity" $ do
        a <> mempty `shouldBe` a

      it "right identity" $ do
        mempty <> a `shouldBe` a
