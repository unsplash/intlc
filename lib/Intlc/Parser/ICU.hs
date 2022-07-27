-- This module follows the following whitespace rules:
--   * Consume all whitespace after tokens where possible.
--   * Therefore, assume no whitespace before tokens.

module Intlc.Parser.ICU where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import qualified Data.Text                                as T
import           Data.These                               (These (..))
import           Data.Void                                ()
import           Intlc.ICU
import           Intlc.Parser.Error                       (MessageParseErr (..),
                                                           ParseErr (FailedMsgParse),
                                                           failingWith)
import           Prelude                                  hiding (Type)
import           Text.Megaparsec                          hiding (State, Stream,
                                                           Token, many, some,
                                                           token)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer               as L

failingWith' :: MonadParsec ParseErr s m => Int -> MessageParseErr -> m a
i `failingWith'` e = i `failingWith` FailedMsgParse e

data ParserState = ParserState
  -- Expected to be supplied internally.
  { pluralCtxName :: Maybe Text
  -- Expected to be potentially supplied externally.
  , endOfInput    :: Parser ()
  }

emptyState :: ParserState
emptyState = ParserState
  { pluralCtxName = Nothing
  , endOfInput = pure ()
  }

type Parser = ReaderT ParserState (Parsec ParseErr Text)

ident :: Parser Text
ident = label "alphabetic identifier" $ T.pack <$> some letterChar

-- Parse a message until the end of input parser matches.
msg :: Parser Message
msg = msgTill =<< asks endOfInput

-- Parse a message until the provided parser matches.
msgTill :: Parser a -> Parser Message
msgTill = fmap (Message . mergePlaintext) . streamTill

-- Parse a stream until the provided parser matches.
streamTill :: Parser a -> Parser Stream
streamTill = manyTill token

-- The core parser of this module. Parse as many of these as you'd like until
-- reaching an anticipated delimiter, such as a double quote in the surrounding
-- JSON string or end of input in a REPL.
token :: Parser Token
token = choice
  [ uncurry Interpolation <$> (interp <|> callback)
  -- Plural cases support interpolating the number/argument in context with
  -- `#`. When there's no such context, fail the parse in effect treating it
  -- as plaintext.
  , asks pluralCtxName >>= \case
      Just n  -> Interpolation n PluralRef <$ string "#"
      Nothing -> empty
  , Plaintext <$> plaintext
  ]

-- Parse plaintext, including single quote escape sequences.
plaintext :: Parser Text
plaintext = choice
  [ try escaped
  , T.singleton <$> L.charLiteral
  ]

-- Follows ICU 4.8+ spec, see:
--   https://unicode-org.github.io/icu/userguide/format_parse/messages/#quotingescaping
escaped :: Parser Text
escaped = apos *> choice
  -- Double escape two apostrophes as one, regardless of surrounding
  -- syntax: "''" -> "'"
  [ "'" <$ apos
  -- Escape everything until another apostrophe, being careful of internal
  -- double escapes: "'{a''}'" -> "{a'}". Must ensure it doesn't surpass the
  -- bounds of the surrounding parser as per `endOfInput`.
  , try $ do
      eom <- asks endOfInput
      head' <- T.singleton <$> synOpen
      -- Try and parse until end of input or a lone apostrophe. If end of input
      -- comes first then fail the parse.
      (tail', wasEom) <- someTill_ plaintext $ choice
        [       True  <$ eom
        , try $ False <$ apos <* notFollowedBy apos
        ]
      guard (not wasEom)
      pure $ head' <> T.concat tail'
  -- Escape the next syntax character as plaintext: "'{" -> "{"
  , T.singleton <$> synAll
  ]
  where apos = char '\''
        synAll = synLone <|> synOpen <|> synClose
        synLone = char '#'
        synOpen = char '{' <|> char '<'
        synClose = char '}' <|> char '>'

callback :: Parser (Text, Type)
callback = do
  (openPos, isClosing, oname) <- (,,) <$> (string "<" *> getOffset) <*> closing <*> ident <* string ">"
  when isClosing $ (openPos + 1) `failingWith'` NoOpeningCallbackTag oname
  mrest <- observing ((,,) <$> children <* string "</" <*> getOffset <*> ident <* string ">")
  case mrest of
    Left _  -> openPos `failingWith'` NoClosingCallbackTag oname
    Right (ch, closePos, cname) -> if oname == cname
       then pure (oname, ch)
       else closePos `failingWith'` BadClosingCallbackTag oname cname
    where children = do
            eom <- asks endOfInput
            stream <- streamTill (lookAhead $ void (string "</") <|> eom)
            pure . Callback . mergePlaintext $ stream
          closing = fmap isJust . hidden . optional . char $ '/'

interp :: Parser (Text, Type)
interp = between (char '{') (char '}') $ do
  n <- ident
  (n,) <$> option String (sep *> body n)
  where sep = string "," <* hspace1
        body n = choice
          [ uncurry Bool <$> (string "boolean" *> sep *> boolCases)
          , Number <$ string "number"
          , Date <$> (string "date" *> sep *> dateTimeFmt)
          , Time <$> (string "time" *> sep *> dateTimeFmt)
          , Plural <$> withPluralCtx n (
                  string "plural" *> sep *> cardinalCases
              <|> string "selectordinal" *> sep *> ordinalCases
            )
          , Select <$> (string "select" *> sep *> selectCases)
          ]
        withPluralCtx n = withReaderT (\x -> x { pluralCtxName = Just n })

dateTimeFmt :: Parser DateTimeFmt
dateTimeFmt = choice
  [ Short  <$ string "short"
  , Medium <$ string "medium"
  , Long   <$ string "long"
  , Full   <$ string "full"
  ]

caseBody :: Parser Stream
caseBody = mergePlaintext <$> (string "{" *> streamTill (string "}"))

boolCases :: Parser (Stream, Stream)
boolCases = (,)
  <$> (string "true"  *> hspace1 *> caseBody)
   <* hspace1
  <*> (string "false" *> hspace1 *> caseBody)

selectCases :: Parser (These (NonEmpty SelectCase) SelectWildcard)
selectCases = choice
  [ reconcile <$> cases <*> optional wildcard
  , That <$> wildcard
  ]
  where cases = NE.sepEndBy1 (SelectCase <$> (name <* hspace1) <*> caseBody) hspace1
        wildcard = SelectWildcard <$> (string wildcardName *> hspace1 *> caseBody)
        reconcile cs (Just w) = These cs w
        reconcile cs Nothing  = This cs
        name = try $ mfilter (/= wildcardName) ident
        wildcardName = "other"

cardinalCases :: Parser Plural
cardinalCases = try cardinalInexactCases <|> cardinalExactCases

cardinalExactCases :: Parser Plural
cardinalExactCases = CardinalExact <$> NE.sepEndBy1 pluralExactCase hspace1

cardinalInexactCases :: Parser Plural
cardinalInexactCases = uncurry CardinalInexact <$> mixedPluralCases <*> pluralWildcard

ordinalCases :: Parser Plural
ordinalCases = uncurry Ordinal <$> mixedPluralCases <*> pluralWildcard

mixedPluralCases :: Parser ([PluralCase PluralExact], [PluralCase PluralRule])
mixedPluralCases = partitionEithers <$> sepEndBy (eitherP pluralExactCase pluralRuleCase) hspace1

pluralExactCase :: Parser (PluralCase PluralExact)
pluralExactCase = PluralCase <$> pluralExact <* hspace1 <*> caseBody
  where pluralExact = PluralExact . T.pack <$> (string "=" *> some numberChar)

pluralRuleCase :: Parser (PluralCase PluralRule)
pluralRuleCase = PluralCase <$> pluralRule <* hspace1 <*> caseBody

pluralRule :: Parser PluralRule
pluralRule = choice
  [ Zero <$ string "zero"
  , One  <$ string "one"
  , Two  <$ string "two"
  , Few  <$ string "few"
  , Many <$ string "many"
  ]

pluralWildcard :: Parser PluralWildcard
pluralWildcard = PluralWildcard <$> (string "other" *> hspace1 *> caseBody)
