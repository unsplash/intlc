-- This module follows the following whitespace rules:
--   * Consume all whitespace after tokens where possible.
--   * Therefore, assume no whitespace before tokens.

{-# LANGUAGE FlexibleContexts #-}

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
                  string "plural" *> sep *> cardinalPluralCases
              <|> string "selectordinal" *> sep *> ordinalPluralCases
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

cardinalPluralCases :: Parser Plural
cardinalPluralCases = tryClassify =<< p
    where tryClassify = maybe empty pure . uncurry classifyCardinal
          p = (,) <$> disorderedPluralCases <*> optional pluralWildcard

ordinalPluralCases :: Parser Plural
ordinalPluralCases = tryClassify =<< p
    where tryClassify = maybe empty pure . uncurry classifyOrdinal
          p = (,) <$> disorderedPluralCases <*> pluralWildcard

-- Need to lift parsed plural cases into this type to make the list homogeneous.
data ParsedPluralCase
  = ParsedExact (PluralCase PluralExact)
  | ParsedRule (PluralCase PluralRule)

disorderedPluralCases :: Parser [ParsedPluralCase]
disorderedPluralCases = flip sepEndBy hspace1 $ choice
  [ (ParsedExact .) . PluralCase <$> pluralExact <* hspace1 <*> caseBody
  , (ParsedRule .)  . PluralCase <$> pluralRule  <* hspace1 <*> caseBody
  ]

pluralExact :: Parser PluralExact
pluralExact = PluralExact . T.pack <$> (string "=" *> some numberChar)

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

-- | To simplify parsing cases we validate after-the-fact here.
classifyCardinal :: Foldable f => f ParsedPluralCase -> Maybe PluralWildcard -> Maybe Plural
classifyCardinal xs mw = case (organisePluralCases xs, mw) of
  ((l:ls, []), Nothing) -> Just (CardinalExact (l:|ls))
  ((ls, rs),   Just w)  -> Just (CardinalInexact ls rs w)
  _                     -> Nothing

-- | Here we need only validate that there is at least one rule case. This is
-- performed here to simplify supporting disordered cases in the parser
-- (whereas validating the presence of a wildcard at the end is trivial in the
-- parser).
classifyOrdinal :: Foldable f => f ParsedPluralCase -> PluralWildcard -> Maybe Plural
classifyOrdinal xs w = case organisePluralCases xs of
  (ls, rs) -> Just $ Ordinal ls rs w

organisePluralCases :: Foldable f => f ParsedPluralCase -> ([PluralCase PluralExact], [PluralCase PluralRule])
organisePluralCases = foldr f mempty
  where f (ParsedExact x) = first (x:)
        f (ParsedRule x)  = second (x:)
