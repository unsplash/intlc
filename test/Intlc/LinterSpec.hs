module Intlc.LinterSpec where
import           Intlc.Parser.ICU
import           Intlc.ICU
import           Intlc.Linter
import           Prelude
import           Test.Hspec
import           Text.Megaparsec       (runParser)
import           Intlc.Parser.Error    (ParseFailure)

parseWith :: ParserState -> Parser a -> Text -> Either ParseFailure a
parseWith s p = runParser (runReaderT p s) "test"

parse :: Parser a -> Text -> Either ParseFailure a
parse = parseWith initialState

lintICUText :: Text -> Either ParseFailure Status
lintICUText t = lint <$> (parse msg t)

spec :: Spec
spec = describe "linter" $ do
  it "lints streams with 1 plain text token" $ do
    lint (Message [Plaintext "yay"]) `shouldBe` Success

  it "lints streams with 2 or more plain text token" $ do
    lint (Message [Plaintext "yay", Plaintext "Hello"]) `shouldBe` Success

  it "lints streams with 1 simple interpolation" $ do
    lint (Message [Interpolation "Hello" String]) `shouldBe` Success

  it "lints streams with 1 complex interpolation" $ do
    lint (Message [Interpolation "Hello" (Callback [])]) `shouldBe` Success

  it "lints streams with 1 complex interpolation and 1 simple interpolation" $ do
    lint (Message [Interpolation "Hello" (Callback []), Plaintext "hello"]) `shouldBe` Success

  it "does not lint streams with 2 or more complex interpolations" $ do
    lint (Message [Interpolation "Hello" (Bool [] []), Interpolation "Hello" (Bool [] [])]) `shouldBe` Failure (pure TooManyInterpolations)

  it "does not lint nested streams" $ do
    lint (Message [Interpolation "outer" (Bool [Interpolation "inner" (Bool [] [])] [])]) `shouldBe` Failure (pure TooManyInterpolations)

  it "does not lint complex interpolations with nested complex interpolations" $ do
    lint (Message [Interpolation "outer" (Select (fromList [SelectCase "hello" [Interpolation "super_inner" (Bool [] [])]]) Nothing)]) `shouldBe` Failure (pure TooManyInterpolations)

  it "lints plural" $ do
    lint (Message $
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
            ]) `shouldBe` Success

  it "does not lint text with emoji" $ do
    lint (Message [Plaintext "Message with an emoji ❤️ 🥺"]) `shouldBe` Failure (pure $ InvalidNonAsciiCharacter(fromList "❤️🥺"))

  it "does not lint text that is deeply nested with emoji" $ do
    lint (Message [Interpolation "Hello" (Bool [] []), Interpolation "Hello" (Bool [Plaintext "Message with an emoji 🥺"] [])]) `shouldBe` Failure (fromList [TooManyInterpolations,InvalidNonAsciiCharacter( fromList ['🥺'])])

  it "lints streams without emoji" $ do
    lint (Message [Plaintext "Text without emoji"]) `shouldBe` Success
    

  it "stops iterating after encountering two stream-interpolations" $ do
    let nested x = Interpolation "x" (Callback [x])
    let e = error "should not reach this item"

    lintWithRules [interpolationsRule] (Message
      [ nested (nested e)
      , e
      ]) `shouldBe` Failure (pure TooManyInterpolations)


  it "lints multiple and nested callback tags" $ do
    lintICUText ("<strong>High <div></div>quality photos</strong> (at least <strong>{megapixels}MP</strong>)") `shouldBe` Right Success
