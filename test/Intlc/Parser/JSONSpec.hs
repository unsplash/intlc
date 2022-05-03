module Intlc.Parser.JSONSpec (spec) where

import           Intlc.Core
import           Intlc.Parser.Error    (ParseFailure)
import           Intlc.Parser.JSON     (dataset)
import           Prelude
import           Test.Hspec
import           Test.Hspec.Megaparsec hiding (initialState)
import           Text.Megaparsec       (runParser)
import           Text.RawString.QQ     (r)

parse :: Text -> Either ParseFailure (Dataset Translation)
parse = runParser dataset "test"

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
