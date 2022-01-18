module Intlc.ParserSpec (spec) where

import           Intlc.ICU
import           Intlc.Parser
import           Prelude               hiding (ByteString)
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec       (ParseErrorBundle, Parsec, parse)
import           Text.Megaparsec.Error (ErrorFancy (ErrorCustom))

parse' :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse' = flip parse "test"

spec :: Spec
spec = describe "parser" $ do
  describe "message" $ do
    it "tolerates unclosed braces" $ do
      parse' msg "a {b} c { d" `shouldParse`
        Dynamic [Plaintext "a ", Interpolation (Arg "b" String), Plaintext " c { d"]

    it "tolerates empty braces" $ do
      parse' msg "a {b} c {} d {e, number}" `shouldParse`
        Dynamic [Plaintext "a ", Interpolation (Arg "b" String), Plaintext " c {} d ", Interpolation (Arg "e" Number)]

  describe "interpolation" $ do
    it "interpolates appropriately" $ do
      parse' interp "{x}" `shouldParse` Arg "x" String

    it "only accepts alphanumeric identifiers" $ do
      parse' interp "{XyZ}" `shouldParse` Arg "XyZ" String
      parse' interp "<XyZ></XyZ>" `shouldParse` Arg "XyZ" (Callback [])
      parse' interp `shouldFailOn` "{x y}"
      parse' interp `shouldFailOn` "<x y></x y>"

    it "validates closing tag name" $ do
      parse' interp "<hello></hello>" `shouldParse` Arg "hello" (Callback [])
      parse' interp `shouldFailOn` "<hello></there>"

  describe "callback" $ do
    it "reports friendly error for bad closing tag" $ do
      let e i = errFancy i . fancy . ErrorCustom

      parse' callback "<hello> there" `shouldFailWith` e 1 (NoClosingCallbackTag "hello")
      parse' callback "<hello> </there>" `shouldFailWith` e 10 (BadClosingCallbackTag "hello" "there")

  describe "plural" $ do
    it "requires at least one non-wildcard case" $ do
      parse' (pluralCases "any") `shouldFailOn` "other {foo}"
      parse' (pluralCases "any") `shouldSucceedOn` "=0 {foo} other {bar}"
      parse' (pluralCases "any") `shouldSucceedOn` "one {foo} other {bar}"

    it "requires a wildcard if there are any rule cases" $ do
      parse' (pluralCases "any") `shouldFailOn`    "=0 {foo} one {bar}"
      parse' (pluralCases "any") `shouldSucceedOn` "=0 {foo} one {bar} other {baz}"
      parse' (pluralCases "any") `shouldSucceedOn` "=0 {foo} =1 {bar}"

    it "parses literal and plural cases, wildcard, and interpolation token" $ do
      parse' (pluralCases "xyz") "=0 {foo} few {bar} other {baz #}" `shouldParse`
        MixedPlural (pure $ PluralCase (PluralExact "0") [Plaintext "foo"]) (pure $ PluralCase Few [Plaintext "bar"]) (PluralWildcard [Plaintext "baz ", Interpolation (Arg "xyz" Number)])
