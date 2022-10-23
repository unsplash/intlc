module Intlc.CompilerSpec (spec) where

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
    let f = compileDataset (Locale "any") . fromList . fmap (, Translation (Message "any") TypeScript Nothing)

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
        [ ("x", Translation (Message "xfoo") TypeScript Nothing)
        , ("z", Translation (Message "zfoo") TypeScriptReact (Just "zbar"))
        , ("y", Translation (Message $ mconcat ["yfoo ", String' "ybar"]) TypeScript Nothing)
        ])
          `shouldBe` [r|{"x":{"message":"xfoo","backend":"ts","description":null},"y":{"message":"yfoo {ybar}","backend":"ts","description":null},"z":{"message":"zfoo","backend":"tsx","description":"zbar"}}|]

    it "escapes double quotes in JSON" $ do
      compileFlattened (fromList [("x\"y", Translation (Message "\"z\"") TypeScript Nothing)])
        `shouldBe` [r|{"x\"y":{"message":"\"z\"","backend":"ts","description":null}}|]

  describe "flatten message" $ do
    it "no-ops static" $ do
      flatten (Message "xyz") `shouldBe` Message "xyz"

    describe "flattens shallow select" $ do
      let foo = ("foo", "a dog")
      let foof = ("foo", "I have a dog")

      it "with a wildcard" $ do
        let other = "many dogs"
        let otherf = "I have many dogs"

        flatten (Message $ mconcat ["I have ", SelectNamedWild' "thing" (pure foo) other]) `shouldBe`
          Message (SelectNamedWild' "thing" (pure foof) otherf)

      it "without a wildcard" $ do
        flatten (Message $ mconcat ["I have ", SelectNamed' "thing" (pure foo)]) `shouldBe`
          Message (SelectNamed' "thing" (pure foof))

    it "flattens shallow plural" $ do
      let other = "many dogs"
      let otherf = "I have many dogs"
      let one = (One, "a dog")
      let onef = (One, "I have a dog")

      flatten (Message $ mconcat ["I have ", CardinalInexact' "count" [] (pure one) other]) `shouldBe`
        Message (CardinalInexact' "count" [] (pure onef) otherf)

    it "flattens deep interpolations" $ do
      let x = Message $ mconcat
            [ "I have "
            , CardinalInexact' "count"
              []
              (pure (One, "a dog"))
              (mconcat [ Number' "count"
              , " dogs, the newest of which is "
              , SelectNamedWild' "name"
                (pure ("hodor", "Hodor"))
                "unknown"
              ])
            , "!"
            ]
      let y = Message $
            CardinalInexact' "count"
              []
              (pure (One, "I have a dog!"))
              (mconcat [ SelectNamedWild' "name"
                (pure ("hodor",
                  mconcat [ "I have "
                  , Number' "count"
                  , " dogs, the newest of which is Hodor!"
                  ]
                ))
                (mconcat [ "I have "
                , Number' "count"
                , " dogs, the newest of which is unknown!"
                ])
              ])

      flatten x `shouldBe` y

  describe "expanding rules" $ do
    let f = expandRules

    it "always contains every rule in the output" $ do
      let c = (,)
      let w = mempty
      let rule (x, _) = x
      let g xs = sort (toList $ rule <$> f xs w)

      g [] `shouldBe` universe
      g [c Zero mempty] `shouldBe` universe
      g [c Many mempty, c Zero mempty] `shouldBe` universe

    it "copies the wildcard node to new rules" $ do
      let xs = "foo"
      let c = (,)
      let w = id
      let g ys = toList (f ys (w xs))

      g [] `shouldBe` (flip c xs <$> (universe :: [PluralRule]))

      g [c Many "bar", c Zero mempty] `shouldBe`
        [c Many "bar", c Zero mempty, c One xs, c Two xs, c Few xs]

    it "returns full list of rules unmodified (as non-empty)" $ do
      let c = (,)
      let xs = [c Two "foo", c Many "", c Zero "bar", c One "baz", c Few ""]

      f xs "any" `shouldBe` fromList xs
