module Intlc.ParserSpec (spec) where

import           Intlc.ICU
import           Intlc.Parser
import           Prelude               hiding (ByteString)
import           Test.Hspec
import           Test.Hspec.Megaparsec hiding (initialState)
import           Text.Megaparsec       (ParseErrorBundle, runParserT)
import           Text.Megaparsec.Error (ErrorFancy (ErrorCustom))

parse' :: Parser a -> Text -> Either (ParseErrorBundle Text MessageParseErr) a
parse' p x = evalState (runParserT p "test" x) initialState

spec :: Spec
spec = describe "parser" $ do
  describe "message" $ do
    it "does not tolerate unclosed braces" $ do
      parse' msg `shouldFailOn` "a { b"

    it "does not tolerate interpolations with a bad type" $ do
      parse' msg `shouldFailOn` "a {n, bool} b"

    it "does not tolerate empty braces" $ do
      parse' msg `shouldFailOn` "a {} b"

    it "does not tolerate empty tags" $ do
      parse' msg `shouldFailOn` "a <> b"

    describe "plural hash" $ do
      it "parses as plaintext outside of plurals" $ do
        parse' msg "#" `shouldParse` Static "#"

      it "parses as arg inside shallow plural" $ do
        let n = pure . Interpolation $ Arg "n" Number
        parse' msg "{n, plural, one {#} other {#}}" `shouldParse`
          (Dynamic . pure . Interpolation . Arg "n" . Plural . Cardinal $
            RulePlural (pure $ PluralCase One n) (PluralWildcard n))

      it "parses as nearest arg inside deep plural" $ do
        let n = pure . Interpolation $ Arg "n" Number
        let i = pure . Interpolation $ Arg "i" Number
        parse' msg "{n, plural, one {{i, plural, one {#} other {#}}} other {#}}" `shouldParse`
          (Dynamic . pure . Interpolation . Arg "n" . Plural . Cardinal $
            RulePlural (pure $ PluralCase One (
              pure . Interpolation . Arg "i" . Plural . Cardinal $
                RulePlural (pure $ PluralCase One i) (PluralWildcard i)
            )) (PluralWildcard n))

    describe "escaping" $ do
      it "escapes non-empty contents between single quotes" $ do
        parse' msg "These are not interpolations: '{word1} {word2}'" `shouldParse`
          Static "These are not interpolations: {word1} {word2}"
        parse' msg "'<notATag>hello</notATag>'" `shouldParse`
          Static "<notATag>hello</notATag>"
        parse' msg "a {b} '{c}' {d} e" `shouldParse`
          Dynamic (Plaintext "a " :| [Interpolation (Arg "b" String), Plaintext " {c} ", Interpolation (Arg "d" String), Plaintext " e"])
        parse' msg "'<f>'" `shouldParse` Static "<f>"
        parse' msg "'<f>x</f>'" `shouldParse` Static "<f>x</f>"
        parse' msg "'<f>x</g>'" `shouldParse` Static "<f>x</g>"

      it "escapes next syntax character following one unclosed single quote" $ do
        parse' msg "This is not an interpolation: '{word}" `shouldParse` Static "This is not an interpolation: {word}"
        parse' msg "'<notATag>" `shouldParse` Static "<notATag>"
        parse' msg "a {b} '{c} {d} e" `shouldParse`
          Dynamic (Plaintext "a " :| [Interpolation (Arg "b" String), Plaintext " {c} ", Interpolation (Arg "d" String), Plaintext " e"])
        parse' msg "a {b} 'c {d} e" `shouldParse`
          Dynamic (Plaintext "a " :| [Interpolation (Arg "b" String), Plaintext " 'c ", Interpolation (Arg "d" String), Plaintext " e"])

      it "escapes two single quotes as one single quote" $ do
        parse' msg "This '{isn''t}' obvious." `shouldParse` Static "This {isn't} obvious."
        parse' msg "a {b} ''{c}'' {d} e" `shouldParse`
          Dynamic (Plaintext "a " :| [Interpolation (Arg "b" String), Plaintext " '", Interpolation (Arg "c" String), Plaintext "' ", Interpolation (Arg "d" String), Plaintext " e"])

      it "ignores one single quote not immediately preceding a syntax character" $ do
        parse' msg "'" `shouldParse` Static "'"
        parse' msg "x'y" `shouldParse` Static "x'y"

  describe "interpolation" $ do
    it "interpolates appropriately" $ do
      parse' interp "{x}" `shouldParse` Arg "x" String

    it "only accepts alphanumeric identifiers" $ do
      parse' interp "{XyZ}" `shouldParse` Arg "XyZ" String
      parse' interp `shouldFailOn` "{x y}"

    it "disallows bad types" $ do
      parse' msg `shouldFailOn` "{n, bool}"
      parse' msg `shouldFailOn` "{n, int, one {x} other {y}}"

    describe "date" $ do
      it "disallows bad formats" $ do
        parse' interp "{x, date, short}" `shouldParse` Arg "x" (Date Short)
        parse' interp `shouldFailOn` "{x, date, miniature}"

    describe "time" $ do
      it "disallows bad formats" $ do
        parse' interp "{x, time, short}" `shouldParse` Arg "x" (Time Short)
        parse' interp `shouldFailOn` "{x, time, miniature}"

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

    it "only accepts alphanumeric identifiers" $ do
      parse' callback "<XyZ></XyZ>" `shouldParse` Arg "XyZ" (Callback [])
      parse' callback `shouldFailOn` "<x y></x y>"

  describe "plural" $ do
    it "disallows wildcard not at the end" $ do
      parse' (cardinalPluralCases "any") `shouldSucceedOn` "=1 {foo} other {bar}"
      parse' (cardinalPluralCases "any") `shouldFailOn` "other {bar} =1 {foo}"

    it "tolerates empty cases" $ do
      parse' (cardinalPluralCases "any") `shouldSucceedOn` "=1 {} other {}"

    it "requires at least one non-wildcard case" $ do
      parse' (cardinalPluralCases "any") `shouldFailOn` "other {foo}"
      parse' (cardinalPluralCases "any") `shouldSucceedOn` "=0 {foo} other {bar}"
      parse' (cardinalPluralCases "any") `shouldSucceedOn` "one {foo} other {bar}"

    it "requires a wildcard if there are any rule cases" $ do
      parse' (cardinalPluralCases "any") `shouldFailOn`    "=0 {foo} one {bar}"
      parse' (cardinalPluralCases "any") `shouldSucceedOn` "=0 {foo} one {bar} other {baz}"
      parse' (cardinalPluralCases "any") `shouldSucceedOn` "=0 {foo} =1 {bar}"

    it "parses literal and plural cases, wildcard, and interpolation token" $ do
      parse' (cardinalPluralCases "xyz") "=0 {foo} few {bar} other {baz #}" `shouldParse`
        Cardinal (MixedPlural (pure $ PluralCase (PluralExact "0") [Plaintext "foo"]) (pure $ PluralCase Few [Plaintext "bar"]) (PluralWildcard [Plaintext "baz ", Interpolation (Arg "xyz" Number)]))

  describe "selectordinal" $ do
    it "disallows wildcard not at the end" $ do
      parse' (ordinalPluralCases "any") `shouldSucceedOn` "one {foo} other {bar}"
      parse' (ordinalPluralCases "any") `shouldFailOn` "other {bar} one {foo}"

    it "tolerates empty cases" $ do
      parse' (ordinalPluralCases "any") `shouldSucceedOn` "one {} other {}"

    it "requires at least one rule" $ do
      parse' (ordinalPluralCases "any") `shouldFailOn` "other {foo}"
      parse' (ordinalPluralCases "any") `shouldSucceedOn` "one {foo} other {bar}"
      parse' (ordinalPluralCases "any") `shouldSucceedOn` "one {foo} two {bar} other {baz}"

    it "requires a wildcard" $ do
      parse' (ordinalPluralCases "any") `shouldFailOn`    "=0 {foo} one {bar}"
      parse' (ordinalPluralCases "any") `shouldSucceedOn` "=0 {foo} one {bar} other {baz}"

    it "parses literal and plural cases, wildcard, and interpolation token" $ do
      parse' (cardinalPluralCases "xyz") "=0 {foo} few {bar} other {baz #}" `shouldParse`
        Cardinal (MixedPlural (pure $ PluralCase (PluralExact "0") [Plaintext "foo"]) (pure $ PluralCase Few [Plaintext "bar"]) (PluralWildcard [Plaintext "baz ", Interpolation (Arg "xyz" Number)]))

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
