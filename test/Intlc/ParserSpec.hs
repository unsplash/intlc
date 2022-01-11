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
  describe "translation" $ do
    it "tolerates unclosed braces" $ do
      parse' translation "a {b} c { d" `shouldParse`
        Dynamic [Plaintext "a ", Interpolation (Arg "b" Nothing), Plaintext " c { d"]
