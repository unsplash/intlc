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
        Dynamic (Plaintext "a " :| [Interpolation (Arg "b" String), Plaintext " c { d"])

    it "tolerates empty braces" $ do
      parse' msg "a {b} c {} d {e, number}" `shouldParse`
        Dynamic (Plaintext "a " :| [Interpolation (Arg "b" String), Plaintext " c {} d ", Interpolation (Arg "e" Number)])

    it "tolerates interpolations with a bad type" $ do
      parse' msg "{n, bool}" `shouldParse` Static "{n, bool}"

    it "does not tolerate empty tags" $ do
      parse' msg `shouldFailOn` "a <> b"

  describe "interpolation" $ do
    it "interpolates appropriately" $ do
      parse' interp "{x}" `shouldParse` Arg "x" String

    it "only accepts alphanumeric identifiers" $ do
      parse' interp "{XyZ}" `shouldParse` Arg "XyZ" String
      parse' interp "<XyZ></XyZ>" `shouldParse` Arg "XyZ" (Callback [])
      parse' interp `shouldFailOn` "{x y}"
      parse' interp `shouldFailOn` "<x y></x y>"

    describe "date" $ do
      it "disallows bad formats" $ do
        parse' interp "{x, date, short}" `shouldParse` Arg "x" (Date Short)
        parse' interp `shouldFailOn` "{x, date, miniature}"

  describe "callback" $ do
    it "parses nested" $ do
      parse' callback "<f><g>x{y}z</g></f>" `shouldParse`
        Arg "f" (Callback [Interpolation $ Arg "g" (Callback [Plaintext "x", Interpolation (Arg "y" String), Plaintext "z"])])

    it "validates closing tag name" $ do
      parse' callback "<hello></hello>" `shouldParse` Arg "hello" (Callback [])
      parse' callback `shouldFailOn` "<hello></there>"

    it "reports friendly error for bad closing tag" $ do
      let e i = errFancy i . fancy . ErrorCustom

      parse' callback "<hello> there" `shouldFailWith` e 1 (NoClosingCallbackTag "hello")
      parse' callback "<hello> </there>" `shouldFailWith` e 10 (BadClosingCallbackTag "hello" "there")

  describe "plural" $ do
    it "disallows wildcard not at the end" $ do
      parse' (pluralCases "any") `shouldSucceedOn` "=1 {foo} other {bar}"
      parse' (pluralCases "any") `shouldFailOn` "other {bar} =1 {foo}"

    it "tolerates empty cases" $ do
      parse' (pluralCases "any") `shouldSucceedOn` "=1 {} other {}"

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

  describe "select" $ do
    it "disallows wildcard not at the end" $ do
      parse' selectCases "foo {bar} other {baz}" `shouldParse` (pure (SelectCase "foo" [Plaintext "bar"]), Just (SelectWildcard [Plaintext "baz"]))
      parse' selectCases `shouldFailOn` "other {bar} foo {baz}"

    it "tolerates empty cases" $ do
      parse' selectCases "x {} other {}" `shouldParse` (pure (SelectCase "x" []), Just (SelectWildcard []))

    it "requires at least one non-wildcard case" $ do
      parse' selectCases "foo {bar}" `shouldParse` (pure (SelectCase "foo" [Plaintext "bar"]), Nothing)
      parse' selectCases "foo {bar} other {baz}" `shouldParse` (pure (SelectCase "foo" [Plaintext "bar"]), Just (SelectWildcard [Plaintext "baz"]))
      parse' selectCases `shouldFailOn` "other {foo}"
