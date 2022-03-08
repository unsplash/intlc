module Intlc.CompilerSpec (spec) where

import           Intlc.Backend.JavaScript.Compiler (emptyModule)
import           Intlc.Compiler                    (compileDataset, flatten)
import           Intlc.Core                        (Locale (Locale))
import           Intlc.ICU
import           Prelude                           hiding (one)
import           Test.Hspec

spec :: Spec
spec = describe "compiler" $ do
  describe "compileDataset" $ do
    it "compiles empty dataset to valid JS module format" $ do
      compileDataset (Locale "foo") mempty `shouldBe` emptyModule

  describe "flatten" $ do
    it "no-ops static" $ do
      flatten (Static "xyz") `shouldBe` Static "xyz"

    describe "flattens shallow select" $ do
      let foo = SelectCase "foo" [Plaintext "a dog"]
      let foof = SelectCase "foo" [Plaintext "I have a dog"]

      it "with a wildcard" $ do
        let other = SelectWildcard [Plaintext "many dogs"]
        let otherf = SelectWildcard [Plaintext "I have many dogs"]

        flatten (Dynamic $ Plaintext "I have " :| [Interpolation (Arg "thing" (Select (pure foo) (pure other)))]) `shouldBe`
          Dynamic (pure $ Interpolation (Arg "thing" (Select (pure foof) (pure otherf))))

      it "without a wildcard" $ do
        flatten (Dynamic $ Plaintext "I have " :| [Interpolation (Arg "thing" (Select (pure foo) empty))]) `shouldBe`
          Dynamic (pure $ Interpolation (Arg "thing" (Select (pure foof) empty)))

    it "flattens shallow plural" $ do
      let other = PluralWildcard [Plaintext "many dogs"]
      let otherf = PluralWildcard [Plaintext "I have many dogs"]
      let one = PluralCase One [Plaintext "a dog"]
      let onef = PluralCase One [Plaintext "I have a dog"]

      flatten (Dynamic $ Plaintext "I have " :| [Interpolation (Arg "count" (Plural (Cardinal (RulePlural (pure one) other))))]) `shouldBe`
        Dynamic (pure $ Interpolation (Arg "count" (Plural (Cardinal (RulePlural (pure onef) otherf)))))

    it "flattens deep interpolations" $ do
      let x = Dynamic $
              Plaintext "I have " :|
            [ Interpolation . Arg "count" . Plural . Cardinal $ RulePlural
              (pure $ PluralCase One [Plaintext "a dog"])
              (PluralWildcard
                [ Interpolation $ Arg "count" Number
                , Plaintext " dogs, the newest of which is "
                , Interpolation . Arg "name" $ Select
                  (pure $ SelectCase "hodor" [Plaintext "Hodor"])
                  (pure $ SelectWildcard [Plaintext "unknown"])
                ]
              )
            , Plaintext "!"
            ]
      let y = Dynamic . pure $
            Interpolation . Arg "count" . Plural . Cardinal $ RulePlural
              (pure $ PluralCase One [Plaintext "I have a dog!"])
              (PluralWildcard
                [ Interpolation . Arg "name" $ Select
                  (pure $ SelectCase "hodor"
                    [ Plaintext "I have "
                    , Interpolation $ Arg "count" Number
                    , Plaintext " dogs, the newest of which is Hodor!"
                    ]
                  )
                  (pure $ SelectWildcard
                    [ Plaintext "I have "
                    , Interpolation $ Arg "count" Number
                    , Plaintext " dogs, the newest of which is unknown!"
                    ]
                  )
                ]
              )

      flatten x `shouldBe` y
