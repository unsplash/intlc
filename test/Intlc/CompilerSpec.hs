module Intlc.CompilerSpec (spec) where

import           Data.These        (These (..))
import           Intlc.Compiler    (compileDataset, compileFlattened,
                                    expandRules, flatten)
import           Intlc.Core        (Backend (..), Locale (Locale),
                                    Translation (Translation))
import           Intlc.ICU
import           Prelude           hiding (one)
import           Test.Hspec
import           Text.RawString.QQ (r)

spec :: Spec
spec = describe "compiler" $ do
  describe "compile" $ do
    let f = compileDataset (Locale "any") . fromList . fmap (, Translation (Message [Plaintext "any"]) TypeScript Nothing)

    it "validates keys don't contain invalid chars" $ do
      f ["goodKey"] `shouldSatisfy` isRight
      f ["bad key"] `shouldSatisfy` isLeft

    it "validates keys aren't reserved words" $ do
      f ["delete"] `shouldSatisfy` isLeft

    it "validates keys aren't empty" $ do
      f [""] `shouldSatisfy` isLeft

  describe "compile flattened dataset" $ do
    it "flattens messages and outputs JSON" $ do
      compileFlattened (fromList
        [ ("x", Translation (Message [Plaintext "xfoo"]) TypeScript Nothing)
        , ("z", Translation (Message [Plaintext "zfoo"]) TypeScriptReact (Just "zbar"))
        , ("y", Translation (Message [Plaintext "yfoo ", String "ybar"]) TypeScript Nothing)
        ])
          `shouldBe` [r|{"x":{"message":"xfoo","backend":"ts","description":null},"y":{"message":"yfoo {ybar}","backend":"ts","description":null},"z":{"message":"zfoo","backend":"tsx","description":"zbar"}}|]

    it "escapes double quotes in JSON" $ do
      compileFlattened (fromList [("x\"y", Translation (Message [Plaintext "\"z\""]) TypeScript Nothing)])
        `shouldBe` [r|{"x\"y":{"message":"\"z\"","backend":"ts","description":null}}|]

  describe "flatten message" $ do
    it "no-ops static" $ do
      flatten (Message [Plaintext "xyz"]) `shouldBe` Message [Plaintext "xyz"]

    describe "flattens shallow select" $ do
      let foo = SelectCase "foo" [Plaintext "a dog"]
      let foof = SelectCase "foo" [Plaintext "I have a dog"]

      it "with a wildcard" $ do
        let other = SelectWildcard [Plaintext "many dogs"]
        let otherf = SelectWildcard [Plaintext "I have many dogs"]

        flatten (Message [Plaintext "I have ", Select "thing" $ These (pure foo) other]) `shouldBe`
          Message (pure . Select "thing" $ These (pure foof) otherf)

      it "without a wildcard" $ do
        flatten (Message [Plaintext "I have ", Select "thing" $ This (pure foo)]) `shouldBe`
          Message (pure . Select "thing" $ This (pure foof))

    it "flattens shallow plural" $ do
      let other = PluralWildcard [Plaintext "many dogs"]
      let otherf = PluralWildcard [Plaintext "I have many dogs"]
      let one = (One, [Plaintext "a dog"])
      let onef = (One, [Plaintext "I have a dog"])

      flatten (Message [Plaintext "I have ", CardinalInexact "count" [] (pure one) other]) `shouldBe`
        Message (pure $ CardinalInexact "count" [] (pure onef) otherf)

    it "flattens deep interpolations" $ do
      let x = Message
            [ Plaintext "I have "
            , CardinalInexact "count"
              []
              (pure (One, [Plaintext "a dog"]))
              (PluralWildcard
                [ Number "count"
                , Plaintext " dogs, the newest of which is "
                , Select "name" $ These
                  (pure $ SelectCase "hodor" [Plaintext "Hodor"])
                  (SelectWildcard [Plaintext "unknown"])
                ]
              )
            , Plaintext "!"
            ]
      let y = Message . pure $
            CardinalInexact "count"
              []
              (pure (One, [Plaintext "I have a dog!"]))
              (PluralWildcard
                [ Select "name" $ These
                  (pure $ SelectCase "hodor"
                    [ Plaintext "I have "
                    , Number "count"
                    , Plaintext " dogs, the newest of which is Hodor!"
                    ]
                  )
                  (SelectWildcard
                    [ Plaintext "I have "
                    , Number "count"
                    , Plaintext " dogs, the newest of which is unknown!"
                    ]
                  )
                ]
              )

      flatten x `shouldBe` y

  describe "expanding rules" $ do
    let f = expandRules

    it "always contains every rule in the output" $ do
      let c = (,)
      let w = PluralWildcard mempty
      let rule (x, _) = x
      let g xs = sort (toList $ rule <$> f xs w)

      g [] `shouldBe` universe
      g [c Zero mempty] `shouldBe` universe
      g [c Many mempty, c Zero mempty] `shouldBe` universe

    it "copies the wildcard stream to new rules" $ do
      let xs = [Plaintext "foo"]
      let c = (,)
      let w = PluralWildcard
      let g ys = toList (f ys (w xs))

      g [] `shouldBe` (flip c xs <$> (universe :: [PluralRule]))

      g [c Many [Plaintext "bar"], c Zero mempty] `shouldBe`
        [c Many [Plaintext "bar"], c Zero mempty, c One xs, c Two xs, c Few xs]

    it "returns full list of rules unmodified (as non-empty)" $ do
      let c x y = (x, [Plaintext y])
      let xs = [c Two "foo", c Many "", c Zero "bar", c One "baz", c Few ""]

      f xs (PluralWildcard [Plaintext "any"]) `shouldBe` fromList xs
