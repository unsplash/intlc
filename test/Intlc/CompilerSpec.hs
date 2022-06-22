module Intlc.CompilerSpec (spec) where

import           Intlc.Compiler    (compileDataset, compileFlattened, flatten)
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
        , ("y", Translation (Message [Plaintext "yfoo ", Interpolation "ybar" String]) TypeScript Nothing)
        ])
          `shouldBe` [r|{"x":{"message":"xfoo","backend":"ts","description":null},"y":{"message":"yfoo {ybar}","backend":"ts","description":null},"z":{"message":"zfoo","backend":"tsx","description":"zbar"}}|]

  describe "flatten message" $ do
    it "no-ops static" $ do
      flatten (Message [Plaintext "xyz"]) `shouldBe` Message [Plaintext "xyz"]

    describe "flattens shallow select" $ do
      let foo = SelectCase "foo" [Plaintext "a dog"]
      let foof = SelectCase "foo" [Plaintext "I have a dog"]

      it "with a wildcard" $ do
        let other = SelectWildcard [Plaintext "many dogs"]
        let otherf = SelectWildcard [Plaintext "I have many dogs"]

        flatten (Message [Plaintext "I have ", Interpolation "thing" (Select (pure foo) (pure other))]) `shouldBe`
          Message (pure $ Interpolation "thing" (Select (pure foof) (pure otherf)))

      it "without a wildcard" $ do
        flatten (Message [Plaintext "I have ", Interpolation "thing" (Select (pure foo) empty)]) `shouldBe`
          Message (pure $ Interpolation "thing" (Select (pure foof) empty))

    it "flattens shallow plural" $ do
      let other = PluralWildcard [Plaintext "many dogs"]
      let otherf = PluralWildcard [Plaintext "I have many dogs"]
      let one = PluralCase One [Plaintext "a dog"]
      let onef = PluralCase One [Plaintext "I have a dog"]

      flatten (Message [Plaintext "I have ", Interpolation "count" (Plural (Cardinal (RulePlural (pure one) other)))]) `shouldBe`
        Message (pure $ Interpolation "count" (Plural (Cardinal (RulePlural (pure onef) otherf))))

    it "flattens deep interpolations" $ do
      let x = Message $
            [ Plaintext "I have "
            , Interpolation "count" . Plural . Cardinal $ RulePlural
              (pure $ PluralCase One [Plaintext "a dog"])
              (PluralWildcard
                [ Interpolation "count" Number
                , Plaintext " dogs, the newest of which is "
                , Interpolation "name" $ Select
                  (pure $ SelectCase "hodor" [Plaintext "Hodor"])
                  (pure $ SelectWildcard [Plaintext "unknown"])
                ]
              )
            , Plaintext "!"
            ]
      let y = Message . pure $
            Interpolation "count" . Plural . Cardinal $ RulePlural
              (pure $ PluralCase One [Plaintext "I have a dog!"])
              (PluralWildcard
                [ Interpolation "name" $ Select
                  (pure $ SelectCase "hodor"
                    [ Plaintext "I have "
                    , Interpolation "count" Number
                    , Plaintext " dogs, the newest of which is Hodor!"
                    ]
                  )
                  (pure $ SelectWildcard
                    [ Plaintext "I have "
                    , Interpolation "count" Number
                    , Plaintext " dogs, the newest of which is unknown!"
                    ]
                  )
                ]
              )

      flatten x `shouldBe` y
