module Intlc.ParserSpec (spec) where

import           Intlc.Core
import           Intlc.Parser
import           Prelude               hiding (ByteString)
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec       (ParseErrorBundle, Parsec, parse)

parse' :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse' = flip parse "test"

spec :: Spec
spec = describe "parser" $ do
  describe "message" $ do
    it "tolerates unclosed braces" $ do
      parse' msg "a {b} c { d" `shouldParse`
        Dynamic [Plaintext "a ", Interpolation (Arg "b" Nothing), Plaintext " c { d"]

    it "tolerates unclosed/unopened tags" $ do
      parse' msg "a <hello> c </there> d" `shouldParse`
        Static "a <hello> c </there> d"

  describe "interpolation" $ do
    it "validates closing tag name" $ do
      parse' interp "<hello></hello>" `shouldParse` Arg "hello" (Just $ Callback [])
      parse' interp `shouldFailOn` "<hello></there>"
