module Intlc.Parser.JSONSpec (spec) where

import           Intlc.Core
import           Intlc.Parser          (parseDataset)
import           Intlc.Parser.Error    (JSONParseErr (..), ParseErr (..),
                                        ParseFailure)
import           Prelude
import           Test.Hspec
import           Test.Hspec.Megaparsec hiding (initialState)
import           Text.Megaparsec       (ErrorFancy (ErrorCustom))
import           Text.RawString.QQ     (r)

parse :: Text -> Either ParseFailure (Dataset Translation)
parse = parseDataset "test"

succeedsOn :: Text -> Expectation
succeedsOn = shouldSucceedOn parse

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
    let e i = errFancy i . fancy . ErrorCustom . FailedJSONParse
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
      [ e 113 (DuplicateKey "b")
      , e 148 (DuplicateKey "b")
      , e 253 (DuplicateKey "e")
      ]
