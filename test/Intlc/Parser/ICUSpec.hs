module Intlc.Parser.ICUSpec (spec) where

import           Data.These            (These (..))
import           Intlc.ICU
import           Intlc.Parser.Error    (MessageParseErr (..),
                                        ParseErr (FailedMsgParse), ParseFailure)
import           Intlc.Parser.ICU
import           Prelude
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec       (eof, runParser)
import           Text.Megaparsec.Error (ErrorFancy (ErrorCustom))

parseWith :: ParserState -> Parser a -> Text -> Either ParseFailure a
parseWith s p = runParser (runReaderT p s) "test"

parse :: Parser a -> Text -> Either ParseFailure a
parse = parseWith $ emptyState { endOfInput = eof }

spec :: Spec
spec = describe "ICU parser" $ do
  describe "message" $ do
    it "does not tolerate unclosed braces" $ do
      parse msg `shouldFailOn` "a { b"

    it "does not tolerate interpolations with a bad type" $ do
      parse msg `shouldFailOn` "a {n, badtype} b"

    it "does not tolerate empty braces" $ do
      parse msg `shouldFailOn` "a {} b"

    it "does not tolerate empty tags" $ do
      parse msg `shouldFailOn` "a <> b"

    describe "plural hash" $ do
      it "parses as plaintext outside of plurals" $ do
        parse msg "#" `shouldParse` Message [Plaintext "#"]
        parse msg "{x, select, y {#}}" `shouldParse`
          (Message . pure . Interpolation "x" $
            Select (This . pure $ SelectCase "y" (pure $ Plaintext "#")))

      it "parses as arg inside shallow plural" $ do
        let n = pure $ Interpolation "n" PluralRef
        parse msg "{n, plural, one {#} other {#}}" `shouldParse`
          (Message . pure . Interpolation "n" . Plural $
            CardinalInexact [] (pure $ PluralCase One n) (PluralWildcard n))

      it "parses as nearest arg inside deep plural" $ do
        let n = pure $ Interpolation "n" PluralRef
        let i = pure $ Interpolation "i" PluralRef
        parse msg "{n, plural, one {{i, plural, one {#} other {#}}} other {#}}" `shouldParse`
          (Message . pure . Interpolation "n" . Plural $
            CardinalInexact [] (pure $ PluralCase One (
              pure . Interpolation "i" . Plural $
                CardinalInexact [] (pure $ PluralCase One i) (PluralWildcard i)
            )) (PluralWildcard n))

      it "parses as arg nested inside other interpolation" $ do
        let n = pure $ Interpolation "n" PluralRef
        parse msg "{n, plural, one {<f>#</f>} other {#}}" `shouldParse`
          (Message . pure . Interpolation "n" . Plural $
            CardinalInexact [] (pure $ PluralCase One (
              pure . Interpolation "f" . Callback $ n
            )) (PluralWildcard n))

    describe "escaping" $ do
      it "escapes non-empty contents between single quotes" $ do
        parse msg "These are not interpolations: '{word1} {word2}'" `shouldParse`
          Message [Plaintext "These are not interpolations: {word1} {word2}"]
        parse msg "'<notATag>hello</notATag>'" `shouldParse`
          Message [Plaintext "<notATag>hello</notATag>"]
        parse msg "a {b} '{c}' {d} e" `shouldParse`
          Message [Plaintext "a ", Interpolation "b" String, Plaintext " {c} ", Interpolation "d" String, Plaintext " e"]
        parse msg "'<f>'" `shouldParse` Message [Plaintext "<f>"]
        parse msg "'<f>x</f>'" `shouldParse` Message [Plaintext "<f>x</f>"]
        parse msg "'<f>x</g>'" `shouldParse` Message [Plaintext "<f>x</g>"]

      it "escapes next syntax character following one unclosed single quote" $ do
        parse msg "This is not an interpolation: '{word}" `shouldParse` Message [Plaintext "This is not an interpolation: {word}"]
        parse msg "'<notATag>" `shouldParse` Message [Plaintext "<notATag>"]
        parse msg "a {b} '{c} {d} e" `shouldParse`
          Message [Plaintext "a ", Interpolation "b" String, Plaintext " {c} ", Interpolation "d" String, Plaintext " e"]
        parse msg "a {b} 'c {d} e" `shouldParse`
          Message [Plaintext "a ", Interpolation "b" String, Plaintext " 'c ", Interpolation "d" String, Plaintext " e"]
        parse msg "{n, plural, =42 {# '#}}" `shouldParse`
          let xs = [Interpolation "n" PluralRef, Plaintext " #"]
           in Message [Interpolation "n" $ Plural (CardinalExact (pure $ PluralCase (PluralExact "42") xs))]

      it "escapes two single quotes as one single quote" $ do
        parse msg "This '{isn''t}' obvious." `shouldParse` Message [Plaintext "This {isn't} obvious."]
        parse msg "a {b} ''{c}'' {d} e" `shouldParse`
          Message [Plaintext "a ", Interpolation "b" String, Plaintext " '", Interpolation "c" String, Plaintext "' ", Interpolation "d" String, Plaintext " e"]

      it "ignores one single quote not immediately preceding a syntax character" $ do
        parse msg "'" `shouldParse` Message [Plaintext "'"]
        parse msg "' '" `shouldParse` Message [Plaintext "' '"]
        parse msg "x'y" `shouldParse` Message [Plaintext "x'y"]

  describe "interpolation" $ do
    it "interpolates appropriately" $ do
      parse interp "{x}" `shouldParse` ("x", String)

    it "only accepts alphanumeric identifiers" $ do
      parse interp "{XyZ}" `shouldParse` ("XyZ", String)
      parse interp `shouldFailOn` "{x y}"

    it "disallows bad types" $ do
      parse msg `shouldFailOn` "{n, enum}"
      parse msg `shouldFailOn` "{n, int, one {x} other {y}}"

    describe "bool" $ do
      it "requires both bool cases" $ do
        parse interp "{x, boolean, true {y} false {z}}" `shouldParse` ("x", Bool [Plaintext "y"] [Plaintext "z"])
        parse interp `shouldFailOn` "{x, boolean, true {y}}"
        parse interp `shouldFailOn` "{x, boolean, false {y}}"

      it "enforces case order" $ do
        parse interp `shouldFailOn` "{x, boolean, false {y} true {z}}"

      it "disallows arbitrary cases" $ do
        parse interp `shouldFailOn` "{x, boolean, true {y} nottrue {z}}"

    describe "date" $ do
      it "disallows bad formats" $ do
        parse interp "{x, date, short}" `shouldParse` ("x", Date Short)
        parse interp `shouldFailOn` "{x, date, miniature}"

    describe "time" $ do
      it "disallows bad formats" $ do
        parse interp "{x, time, short}" `shouldParse` ("x", Time Short)
        parse interp `shouldFailOn` "{x, time, miniature}"

  describe "callback" $ do
    let e i = errFancy i . fancy . ErrorCustom . FailedMsgParse

    it "parses nested" $ do
      parse callback "<f><g>x{y}z</g></f>" `shouldParse`
        ("f", Callback [Interpolation "g" (Callback [Plaintext "x", Interpolation "y" String, Plaintext "z"])])

    it "requires closing tag" $ do
      parse callback "<hello> there" `shouldFailWith` e 1 (NoClosingCallbackTag "hello")

    it "requires opening tag" $ do
      parse callback "</hello> <there>" `shouldFailWith` e 2 (NoOpeningCallbackTag "hello")

    it "validates closing tag name" $ do
      parse callback "<hello></hello>" `shouldParse` ("hello", Callback [])
      parse callback "<hello></there>" `shouldFailWith` e 9 (BadClosingCallbackTag "hello" "there")

    it "only accepts alphanumeric identifiers" $ do
      parse callback "<XyZ></XyZ>" `shouldParse` ("XyZ", Callback [])
      parse callback `shouldFailOn` "<x y></x y>"

  describe "plural" $ do
    let cardinalCases' = cardinalCases <* eof

    it "disallows wildcard not at the end" $ do
      parse cardinalCases' `shouldSucceedOn` "=1 {foo} other {bar}"
      parse cardinalCases' `shouldFailOn` "other {bar} =1 {foo}"

    it "tolerates empty cases" $ do
      parse cardinalCases' `shouldSucceedOn` "=1 {} other {}"

    it "tolerates no non-wildcard cases" $ do
      parse cardinalCases' `shouldSucceedOn` "other {foo}"

    it "requires a wildcard if there are any rule cases" $ do
      parse cardinalCases' `shouldFailOn`    "=0 {foo} one {bar}"
      parse cardinalCases' `shouldSucceedOn` "=0 {foo} one {bar} other {baz}"
      parse cardinalCases' `shouldSucceedOn` "=0 {foo} =1 {bar}"

    it "parses literal and plural cases, wildcard, and interpolation token" $ do
      parseWith (emptyState { pluralCtxName = Just "xyz" }) cardinalCases' "=0 {foo} few {bar} other {baz #}" `shouldParse`
        CardinalInexact (pure $ PluralCase (PluralExact "0") [Plaintext "foo"]) (pure $ PluralCase Few [Plaintext "bar"]) (PluralWildcard [Plaintext "baz ", Interpolation "xyz" PluralRef])

  describe "selectordinal" $ do
    let ordinalCases' = ordinalCases <* eof

    it "disallows wildcard not at the end" $ do
      parse ordinalCases' `shouldSucceedOn` "one {foo} other {bar}"
      parse ordinalCases' `shouldFailOn` "other {bar} one {foo}"

    it "tolerates empty cases" $ do
      parse ordinalCases' `shouldSucceedOn` "one {} other {}"

    it "tolerates no non-wildcard cases" $ do
      parse ordinalCases' `shouldSucceedOn` "other {foo}"

    it "requires a wildcard" $ do
      parse ordinalCases' `shouldFailOn`    "=0 {foo} one {bar}"
      parse ordinalCases' `shouldSucceedOn` "=0 {foo} one {bar} other {baz}"

    it "parses literal and plural cases, wildcard, and interpolation token" $ do
      parseWith (emptyState { pluralCtxName = Just "xyz" }) ordinalCases' "=0 {foo} few {bar} other {baz #}" `shouldParse`
        Ordinal (pure $ PluralCase (PluralExact "0") [Plaintext "foo"]) (pure $ PluralCase Few [Plaintext "bar"]) (PluralWildcard [Plaintext "baz ", Interpolation "xyz" PluralRef])

  describe "select" $ do
    let selectCases' = selectCases <* eof

    it "disallows wildcard not at the end" $ do
      parse selectCases' "foo {bar} other {baz}" `shouldParse` These (pure $ SelectCase "foo" [Plaintext "bar"]) (SelectWildcard [Plaintext "baz"])
      parse selectCases' `shouldFailOn` "other {bar} foo {baz}"

    it "tolerates empty cases" $ do
      parse selectCases' "x {} other {}" `shouldParse` These (pure $ SelectCase "x" []) (SelectWildcard [])

    it "allows no non-wildcard case" $ do
      parse selectCases' "foo {bar}" `shouldParse` This (pure $ SelectCase "foo" [Plaintext "bar"])
      parse selectCases' "foo {bar} other {baz}" `shouldParse` These (pure $ SelectCase "foo" [Plaintext "bar"]) (SelectWildcard [Plaintext "baz"])
      parse selectCases' "other {foo}" `shouldParse` That (SelectWildcard [Plaintext "foo"])
