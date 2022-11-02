module Intlc.Parser.ICUSpec (spec) where

import           Control.Comonad.Cofree (Cofree ((:<)))
import           Intlc.ICU
import           Intlc.Parser.Error     (MessageParseErr (..),
                                         ParseErr (FailedMsgParse),
                                         ParseFailure)
import           Intlc.Parser.ICU
import           Prelude
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec        (eof, runParser)
import           Text.Megaparsec.Error  (ErrorFancy (ErrorCustom))

-- | Offset parsing won't generally be tested in this module as it just adds
-- noise.
--
-- Most of our parsers return functions awaiting their next sibling. This ties
-- that knot with a nonsense offset.
nonsenseAnn :: NodeF AnnNode -> AnnNode
nonsenseAnn = (-1 :<)

sansAnn' :: NodeF AnnNode -> Node
sansAnn' = sansAnn . nonsenseAnn

fin :: AnnNode
fin = nonsenseAnn Fin

runParserWith :: ParserState -> Parser a -> Text -> Either ParseFailure a
runParserWith s p = runParser (runReaderT p s) "test"

parseWith :: ParserState -> Parser (NodeF AnnNode) -> Text -> Either ParseFailure Node
parseWith s p = runParserWith s (sansAnn' <$> p)

parse :: Parser a -> Text -> Either ParseFailure a
parse = runParserWith $ emptyState { endOfInput = eof }

parseF :: Parser (AnnNode -> NodeF AnnNode) -> Text -> Either ParseFailure Node
-- The most satisfying line of code I've ever written:
parseF = parse . fmap sansAnn' . flip flap fin

-- | Message parser sans annotations.
msg :: Parser (Message Node)
msg = fmap sansAnn <$> annMsg

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
        parse msg "#" `shouldParse` Message "#"
        parse msg "{x, select, y {#}}" `shouldParse`
          Message (SelectNamed' "x" (pure ("y", "#")))

      it "parses as arg inside shallow plural" $ do
        let n = PluralRef' "n"
        parse msg "{n, plural, one {#} other {#}}" `shouldParse`
          Message (CardinalInexact' "n" [] (pure (One, n)) n)

      it "parses as nearest arg inside deep plural" $ do
        let n = PluralRef' "n"
        let i = PluralRef' "i"
        parse msg "{n, plural, one {{i, plural, one {#} other {#}}} other {#}}" `shouldParse`
          Message (CardinalInexact' "n" [] (pure (One, CardinalInexact' "i" [] (pure (One, i)) i)) n)

      it "parses as arg nested inside other interpolation" $ do
        let n = PluralRef' "n"
        parse msg "{n, plural, one {<f>#</f>} other {#}}" `shouldParse`
          Message (CardinalInexact' "n" [] (pure (One, Callback' "f" n)) n)

    describe "escaping" $ do
      it "escapes non-empty contents between single quotes" $ do
        parse msg "These are not interpolations: '{word1} {word2}'" `shouldParse`
          Message "These are not interpolations: {word1} {word2}"
        parse msg "'<notATag>hello</notATag>'" `shouldParse`
          Message "<notATag>hello</notATag>"
        parse msg "a {b} '{c}' {d} e" `shouldParse`
          Message (mconcat ["a ", String' "b", " {c} ", String' "d", " e"])
        parse msg "'<f>'" `shouldParse` Message "<f>"
        parse msg "'<f>x</f>'" `shouldParse` Message "<f>x</f>"
        parse msg "'<f>x</g>'" `shouldParse` Message "<f>x</g>"

      it "escapes next syntax character following one unclosed single quote" $ do
        parse msg "This is not an interpolation: '{word}" `shouldParse`
          Message "This is not an interpolation: {word}"
        parse msg "'<notATag>" `shouldParse` Message "<notATag>"
        parse msg "a {b} '{c} {d} e" `shouldParse`
          Message (mconcat ["a ", String' "b", " {c} ", String' "d", " e"])
        parse msg "a {b} 'c {d} e" `shouldParse`
          Message (mconcat ["a ", String' "b", " 'c ", String' "d", " e"])
        parse msg "{n, plural, =42 {# '#}}" `shouldParse`
          let xs = mconcat [PluralRef' "n", " #"]
           in Message $ mconcat [CardinalExact' "n" (pure (PluralExact "42", xs))]

      it "escapes two single quotes as one single quote" $ do
        parse msg "This '{isn''t}' obvious." `shouldParse` Message "This {isn't} obvious."
        parse msg "a {b} ''{c}'' {d} e" `shouldParse`
          Message (mconcat ["a ", String' "b", " '", String' "c", "' ", String' "d", " e"])

      it "ignores one single quote not immediately preceding a syntax character" $ do
        parse msg "'" `shouldParse` Message "'"
        parse msg "' '" `shouldParse` Message "' '"
        parse msg "x'y" `shouldParse` Message "x'y"

    -- As you can see this isn't fun to write out by hand for tests. This one
    -- test can suffice for ensuring we're parsing annotations correctly.
    it "parses with annotations" $ do
      parse annMsg "Hello' {n, plural, one {{name}} other {}}!" `shouldParse` Message
        ( 0 :< Char 'H' (
          1 :< Char 'e' (
          2 :< Char 'l' (
          3 :< Char 'l' (
          4 :< Char 'o' (
          5 :< Char '\'' (
          6 :< Char ' ' (
          7 :< CardinalInexact "n"
            mempty
            (pure (One, 24 :< String "name" (30 :< Fin)))
            (39 :< Fin) (
          41 :< Char '!' (
          42 :< Fin
        ))))))))))

  describe "interpolation" $ do
    it "interpolates appropriately" $ do
      parseF interp "{x}" `shouldParse` String' "x"

    it "only accepts alphanumeric identifiers" $ do
      parseF interp "{XyZ}" `shouldParse` String' "XyZ"
      parseF interp `shouldFailOn` "{x y}"

    it "disallows bad types" $ do
      parse msg `shouldFailOn` "{n, enum}"
      parse msg `shouldFailOn` "{n, int, one {x} other {y}}"

    describe "bool" $ do
      it "requires both bool cases" $ do
        parseF interp "{x, boolean, true {y} false {z}}" `shouldParse` Bool' "x" "y" "z"
        parseF interp `shouldFailOn` "{x, boolean, true {y}}"
        parseF interp `shouldFailOn` "{x, boolean, false {y}}"

      it "enforces case order" $ do
        parseF interp `shouldFailOn` "{x, boolean, false {y} true {z}}"

      it "disallows arbitrary cases" $ do
        parseF interp `shouldFailOn` "{x, boolean, true {y} nottrue {z}}"

    describe "date" $ do
      it "disallows bad formats" $ do
        parseF interp "{x, date, short}" `shouldParse` Date' "x" Short
        parseF interp `shouldFailOn` "{x, date, miniature}"

    describe "time" $ do
      it "disallows bad formats" $ do
        parseF interp "{x, time, short}" `shouldParse` Time' "x" Short
        parseF interp `shouldFailOn` "{x, time, miniature}"

  describe "callback" $ do
    let e i = errFancy i . fancy . ErrorCustom . FailedMsgParse

    it "parses nested" $ do
      parseF callback "<f><g>x{y}z</g></f>" `shouldParse`
        Callback' "f" (Callback' "g" (mconcat ["x", String' "y", "z"]))

    it "requires closing tag" $ do
      parseF callback "<hello> there" `shouldFailWith` e 1 (NoClosingCallbackTag "hello")

    it "requires opening tag" $ do
      parseF callback "</hello> <there>" `shouldFailWith` e 2 (NoOpeningCallbackTag "hello")

    it "validates closing tag name" $ do
      parseF callback "<hello></hello>" `shouldParse` Callback' "hello" mempty
      parseF callback "<hello></there>" `shouldFailWith` e 9 (BadClosingCallbackTag "hello" "there")

    it "only accepts alphanumeric identifiers" $ do
      parseF callback "<XyZ></XyZ>" `shouldParse` Callback' "XyZ" mempty
      parseF callback `shouldFailOn` "<x y></x y>"

  describe "plural" $ do
    let cardinalCases' = cardinalCases "arg" <* eof

    it "disallows wildcard not at the end" $ do
      parseF cardinalCases' `shouldSucceedOn` "=1 {foo} other {bar}"
      parseF cardinalCases' `shouldFailOn` "other {bar} =1 {foo}"

    it "tolerates empty cases" $ do
      parseF cardinalCases' `shouldSucceedOn` "=1 {} other {}"

    it "tolerates no non-wildcard cases" $ do
      parseF cardinalCases' `shouldSucceedOn` "other {foo}"

    it "requires a wildcard if there are any rule cases" $ do
      parseF cardinalCases' `shouldFailOn`    "=0 {foo} one {bar}"
      parseF cardinalCases' `shouldSucceedOn` "=0 {foo} one {bar} other {baz}"
      parseF cardinalCases' `shouldSucceedOn` "=0 {foo} =1 {bar}"

    it "parses literal and plural cases, wildcard, and interpolation node" $ do
      parseWith (emptyState { pluralCtxName = Just "xyz" }) (cardinalCases' ?? fin) "=0 {foo} few {bar} other {baz #}" `shouldParse`
        CardinalInexact' "arg" (pure (PluralExact "0", "foo")) (pure (Few, "bar")) (mconcat ["baz ", PluralRef' "xyz"])

  describe "selectordinal" $ do
    let ordinalCases' = ordinalCases "arg" <* eof

    it "disallows wildcard not at the end" $ do
      parseF ordinalCases' `shouldSucceedOn` "one {foo} other {bar}"
      parseF ordinalCases' `shouldFailOn` "other {bar} one {foo}"

    it "tolerates empty cases" $ do
      parseF ordinalCases' `shouldSucceedOn` "one {} other {}"

    it "tolerates no non-wildcard cases" $ do
      parseF ordinalCases' `shouldSucceedOn` "other {foo}"

    it "requires a wildcard" $ do
      parseF ordinalCases' `shouldFailOn`    "=0 {foo} one {bar}"
      parseF ordinalCases' `shouldSucceedOn` "=0 {foo} one {bar} other {baz}"

    it "parses literal and plural cases, wildcard, and interpolation node" $ do
      parseWith (emptyState { pluralCtxName = Just "xyz" }) (ordinalCases' ?? fin) "=0 {foo} few {bar} other {baz #}" `shouldParse`
        Ordinal' "arg" (pure (PluralExact "0", "foo")) (pure (Few, "bar")) (mconcat ["baz ", PluralRef' "xyz"])

  describe "select" $ do
    let selectCases' = selectCases "arg" <* eof

    it "disallows wildcard not at the end" $ do
      parseF selectCases' "foo {bar} other {baz}" `shouldParse`
        SelectNamedWild' "arg" (pure ("foo", "bar")) "baz"
      parseF selectCases' `shouldFailOn` "other {bar} foo {baz}"

    it "tolerates empty cases" $ do
      parseF selectCases' "x {} other {}" `shouldParse` SelectNamedWild' "arg" (pure ("x", mempty)) mempty

    it "allows no non-wildcard case" $ do
      parseF selectCases' "foo {bar}" `shouldParse` SelectNamed' "arg" (pure ("foo", "bar"))
      parseF selectCases' "foo {bar} other {baz}" `shouldParse`
        SelectNamedWild' "arg" (pure ("foo", "bar")) "baz"
      parseF selectCases' "other {foo}" `shouldParse` SelectWild' "arg" "foo"
