module Intlc.LinterSpec where

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
        lint (Message (SelectNamed' "x" (pure ("y", mempty))))
          `shouldBe` Success
        lint (Message (SelectNamedWild' "x" (pure ("y", mempty)) mempty))
          `shouldBe` Success

      it "fails on selects with only a wildcard" $ do
        let s = SelectWild'

        lint (Message $ mconcat [s "x" (s "y" mempty), s "z" mempty])
          `shouldBe` Failure (RedundantSelect <$> ("x" :| ["y", "z"]))

    describe "redundant plural" $ do
      let lint = lintWith' redundantPluralRule

      it "succeeds on exact cardinal plural" $ do
        lint (Message $ CardinalExact' "x" (pure (PluralExact "42", mempty)))
          `shouldBe` Success

      it "succeeds on ordinal plural with any non-wildcard case" $ do
        lint (Message $ Ordinal' "x" [(PluralExact "42", mempty)] [] mempty)
          `shouldBe` Success
        lint (Message $ Ordinal' "x" [] [(Two, mempty)] mempty)
          `shouldBe` Success

      it "succeeds on inexact cardinal plural with any non-wildcard case" $ do
        lint (Message $ CardinalInexact' "x" [(PluralExact "42", mempty)] [] mempty)
          `shouldBe` Success
        lint (Message $ CardinalInexact' "x" [] [(Two, mempty)] mempty)
          `shouldBe` Success

      it "fails on ordinal plural with only a wildcard" $ do
        lint (Message $ Callback' "y" (Ordinal' "x" [] [] mempty))
          `shouldBe` Failure (pure $ RedundantPlural "x")

      it "fails on inexact cardinal plural with only a wildcard" $ do
        lint (Message $ Callback' "y" (CardinalInexact' "x" [] [] mempty))
          `shouldBe` Failure (pure $ RedundantPlural "x")

    describe "duplicate select case" $ do
      let lint = lintWith' duplicateSelectCasesRule

      it "reports each duplicate after the first" $ do
        let x = Message $ mconcat
              [ SelectNamed' "a" (fromList
                [ ("a1", mempty)
                , ("a2", mempty)
                , ("a1", mempty)
                , ("a1", mempty)
                , ("a3", SelectNamedWild' "aa" (fromList
                    [ ("aa1", mempty)
                    , ("aa1", mempty)
                    ])
                    mempty)
                , ("a2", mempty)
                , ("a1", mempty)
                ])
              , SelectNamedWild' "b" (fromList
                [ ("b1", mempty)
                , ("b2", mempty)
                , ("b3", mempty)
                , ("b2", SelectNamed' "bb" (fromList
                    [ ("bb1", mempty)
                    ]))
                ])
                mempty
              ]
        lint x `shouldBe` Failure (fromList
          [ DuplicateSelectCase "a" "a1"
          , DuplicateSelectCase "a" "a1"
          , DuplicateSelectCase "a" "a2"
          , DuplicateSelectCase "a" "a1"
          , DuplicateSelectCase "aa" "aa1"
          , DuplicateSelectCase "b" "b2"
          ])

    describe "duplicate plural case" $ do
      let lint = lintWith' duplicatePluralCasesRule

      it "reports each duplicate after the first" $ do
        let x = Message $ mconcat
              [ CardinalExact' "a" (fromList
                [ ("a1", mempty)
                , ("a2", mempty)
                , ("a1", mempty)
                , ("a1", mempty)
                , ("a3", Ordinal' "aa"
                    [ ("aa1", mempty)
                    , ("aa1", mempty)
                    ]
                    [ (One, mempty)
                    , (Many, mempty)
                    , (One, mempty)
                    ]
                    mempty)
                , ("a2", mempty)
                , ("a1", mempty)
                ])
              , CardinalInexact' "b"
                [ ("b1", mempty)
                , ("b2", mempty)
                , ("b3", mempty)
                , ("b2", mempty)
                ]
                [ (One, mempty)
                , (Two, mempty)
                , (Zero, mempty)
                , (Two, mempty)
                ]
                mempty
              ]
        lint x `shouldBe` Failure (fromList
          [ DuplicatePluralCase "a" "=a1"
          , DuplicatePluralCase "a" "=a1"
          , DuplicatePluralCase "a" "=a2"
          , DuplicatePluralCase "a" "=a1"
          , DuplicatePluralCase "aa" "=aa1"
          , DuplicatePluralCase "aa" "one"
          , DuplicatePluralCase "b" "=b2"
          , DuplicatePluralCase "b" "two"
          ])

  describe "internal" $ do
    describe "unicode" $ do
      let lint = lintWith' unsupportedUnicodeRule

      it "does not lint text with emoji" $ do
        lint (Message "Message with an emoji ❤️ 🥺")
          `shouldBe` Failure (InvalidNonAsciiCharacter <$> fromList "❤️🥺")

      it "does not lint text that is deeply nested with emoji" $ do
        lint (Message $ mconcat [Callback' "Hello" mempty, Bool' "Hello" "Message with an emoji 🥺" mempty])
          `shouldBe` Failure (InvalidNonAsciiCharacter <$> fromList "🥺")

      it "lints streams without emoji" $ do
        lint (Message "Text without emoji") `shouldBe` Success

    describe "interpolations" $ do
      let lint = lintWith' interpolationsRule
      -- An example interpolation that's affected by this lint rule.
      let f = SelectWild'

      it "lints streams with no interpolations" $ do
        lint (Message "hello world") `shouldBe` Success

      it "lints streams with 1 simple interpolation" $ do
        lint (Message (String' "Hello")) `shouldBe` Success

      it "lints streams with 1 complex interpolation" $ do
        lint (Message (f "Hello" mempty)) `shouldBe` Success

      it "lints streams with 1 complex interpolation and 1 simple interpolation" $ do
        lint (Message $ mconcat [f "Hello" mempty, "hello"]) `shouldBe` Success

      it "lints plurals and callbacks" $ do
        let cb = flip Callback' mempty
        lint (Message $ mconcat [cb "x", cb "y"]) `shouldBe` Success

        let p n = Ordinal' n [] (pure (Zero, mempty)) mempty
        lint (Message $ mconcat [p "x", p "y"]) `shouldBe` Success

      it "does not lint streams with 2 or more complex interpolations" $ do
        lint (Message $ mconcat [f "x" mempty, f "y" mempty])
          `shouldBe` Failure (pure $ TooManyInterpolations ("x" :| ["y"]))
        lint (Message $ mconcat [f "x" mempty, f "y" mempty, f "z" mempty])
          `shouldBe` Failure (pure $ TooManyInterpolations ("x" :| ["y", "z"]))

      it "does not lint nested streams" $ do
        lint (Message $ mconcat [f "outer" (f "inner" mempty)])
          `shouldBe` Failure (pure $ TooManyInterpolations ("outer" :| ["inner"]))
