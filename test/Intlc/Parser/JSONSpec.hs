module Intlc.Parser.JSONSpec (spec) where

import           Intlc.Core
import qualified Intlc.ICU             as ICU
import           Intlc.Parser          (parseDataset)
import           Intlc.Parser.Error    (JSONParseErr (..), MessageParseErr (..),
                                        ParseErr (..), ParseFailure)
import           Prelude
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec       (ErrorFancy (ErrorCustom), ParseError)
import           Text.RawString.QQ     (r)

parse :: Text -> Either ParseFailure (Dataset Translation)
parse = parseDataset "test"

succeedsOn :: Text -> Expectation
succeedsOn = shouldSucceedOn parse

e :: Int -> ParseErr -> ParseError s ParseErr
e i = errFancy i . fancy . ErrorCustom

spec :: Spec
spec = describe "JSON parser" $ do
  it "parses multiple translations" $ do
    succeedsOn [r|{ "f": { "message": "{foo}" }, "g": { "message": "{bar}" } }|]

  it "parses translation data keys in any order" $ do
    succeedsOn [r|{ "f": { "message": "{foo}", "backend": "ts", "description": "bar" } }|]
    succeedsOn [r|{ "f": { "message": "{foo}", "backend": "ts" } }|]
    succeedsOn [r|{ "f": { "message": "{foo}", "description": "bar", "backend": "ts" } }|]
    succeedsOn [r|{ "f": { "message": "{foo}", "description": "bar" } }|]
    succeedsOn [r|{ "f": { "backend": "ts", "message": "{foo}", "description": "bar" } }|]
    succeedsOn [r|{ "f": { "backend": "ts", "message": "{foo}" } }|]
    succeedsOn [r|{ "f": { "backend": "ts", "description": "bar", "message": "{foo}" } }|]
    succeedsOn [r|{ "f": { "description": "bar", "message": "{foo}", "backend": "ts" } }|]
    succeedsOn [r|{ "f": { "description": "bar", "message": "{foo}" } }|]
    succeedsOn [r|{ "f": { "description": "bar", "backend": "ts", "message": "{foo}" } }|]

  it "accepts null or absence for optional keys" $ do
    succeedsOn [r|{ "f": { "message": "{foo}", "backend": null, "description": null } }|]
    succeedsOn [r|{ "f": { "message": "{foo}" } }|]

  it "rejects duplicate keys" $ do
    parse [r|{
      "a": { "message": "{foo}" },
      "b": { "message": "{foo}" },
      "c": { "message": "{foo}" },
      "b": { "message": "{foo}" },
      "b": { "message": "{foo}" },
      "d": { "message": "{foo}" },
      "e": { "message": "{foo}" },
      "e": { "message": "{foo}" }
    }|] `shouldFailWithM`
      [ e 113 (FailedJSONParse $ DuplicateKey "b")
      , e 148 (FailedJSONParse $ DuplicateKey "b")
      , e 253 (FailedJSONParse $ DuplicateKey "e")
      ]

  it "reports all custom error types simultaneously" $ do
    parse [r|{
      "dupeKey": {
        "message": ""
      },
      "noClosing": {
        "message": "<foo>bar"
      },
      "wrongClosing": {
        "message": "<foo></bar>"
      },
      "dupeKey": {
        "message": ""
      },
      "ok": {
        "message": "<f>{n, number}</f>"
      }
    }|] `shouldFailWithM`
      [ e 94 (FailedMsgParse $ NoClosingCallbackTag "foo")
      , e 163 (FailedMsgParse $ BadClosingCallbackTag "foo" "bar")
      , e 184 (FailedJSONParse $ DuplicateKey "dupeKey")
      ]

  it "doesn't parse interpolation escapes across message boundaries" $ do
    let msg x = Translation { message = ICU.Message x, backend = TypeScript, mdesc = Nothing }

    parse [r|{
      "x": { "message": "a'" },
      "y": { "message": "'b" }
    }|] `shouldParse` fromList
      [ ("x", msg [ICU.Plaintext "a'"])
      , ("y", msg [ICU.Plaintext "'b"])
      ]

    parse [r|{
      "x": { "message": "a'{b" },
      "y": { "message": "c}'d" }
    }|] `shouldParse` fromList
      [ ("x", msg [ICU.Plaintext "a{b"])
      , ("y", msg [ICU.Plaintext "c}'d"])
      ]
