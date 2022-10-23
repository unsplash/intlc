-- This module follows the following whitespace rules:
--   * Consume all whitespace after nodes where possible.
--   * Therefore, assume no whitespace before nodes.

module Intlc.Parser.ICU where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import qualified Data.Text                                as T
import           Data.Void                                ()
import           Intlc.ICU
import           Intlc.Parser.Error                       (MessageParseErr (..),
                                                           ParseErr (FailedMsgParse),
                                                           failingWith)
import           Prelude
import           Text.Megaparsec                          hiding (State, Stream,
                                                           many, some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer               as L

failingWith' :: MonadParsec ParseErr s m => Int -> MessageParseErr -> m a
i `failingWith'` e = i `failingWith` FailedMsgParse e

data ParserState = ParserState
  -- Expected to be supplied internally.
  { pluralCtxName :: Maybe Arg
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

arg :: Parser Arg
arg = Arg <$> ident

-- | Parse a message until end of input.
--
-- To instead parse a message as part of a broader data structure, instead look
-- at `msg` and its `endOfInput` state property.
msg' :: Parsec ParseErr Text Message
msg' = runReaderT msg cfg where
  cfg = emptyState { endOfInput = eof }

-- Parse a message until the end of input parser matches.
msg :: Parser Message
msg = msgTill =<< asks endOfInput

-- Parse a message until the provided parser matches.
msgTill :: Parser a -> Parser Message
msgTill = fmap Message . nodesTill

-- Parse as many `Node`s as possible until the provided parser matches.
nodesTill :: Parser a -> Parser Node
nodesTill end = go where
  go = (Fin <$ end) <|> (node <*> go)

-- The core parser of this module. Parse as many of these as you'd like until
-- reaching an anticipated delimiter, such as a double quote in the surrounding
-- JSON string or end of input in a REPL.
node :: Parser (Node -> Node)
node = choice
  [ interp
  , callback
  -- Plural cases support interpolating the number/argument in context with
  -- `#`. When there's no such context, fail the parse in effect treating it
  -- as plaintext.
  , asks pluralCtxName >>= \case
      Just n  -> PluralRef n <$ string "#"
      Nothing -> empty
  , plaintext
  ]

-- Parse a character or a potentially larger escape sequence.
plaintext :: Parser (Node -> Node)
plaintext = choice
  [ try escaped
  , Char <$> L.charLiteral
  ]

-- Follows ICU 4.8+ spec, see:
--   https://unicode-org.github.io/icu/userguide/format_parse/messages/#quotingescaping
escaped :: Parser (Node -> Node)
escaped = apos *> choice
  -- Double escape two apostrophes as one, regardless of surrounding
  -- syntax: "''" -> "'"
  [ Char <$> apos
  -- Escape everything until another apostrophe, being careful of internal
  -- double escapes: "'{a''}'" -> "{a'}". Must ensure it doesn't surpass the
  -- bounds of the surrounding parser as per `endOfInput`.
  , try $ do
      eom <- asks endOfInput
      head' <- Char <$> synOpen
      -- Try and parse until end of input or a lone apostrophe. If end of input
      -- comes first then fail the parse.
      (tail', wasEom) <- someTill_ plaintext $ choice
        [       True  <$ eom
        , try $ False <$ apos <* notFollowedBy apos
        ]
      guard (not wasEom)
      pure . foldr (.) id $ head' : tail'
  -- Escape the next syntax character as plaintext: "'{" -> "{"
  , Char <$> synAll
  ]
  where apos = char '\''
        synAll = synLone <|> synOpen <|> synClose
        synLone = char '#'
        synOpen = char '{' <|> char '<'
        synClose = char '}' <|> char '>'

callback :: Parser (Node -> Node)
callback = do
  (openPos, isClosing, oname) <- (,,) <$> (string "<" *> getOffset) <*> closing <*> arg <* string ">"
  when isClosing $ (openPos + 1) `failingWith'` NoOpeningCallbackTag oname
  mrest <- observing ((,,) <$> children oname <* string "</" <*> getOffset <*> arg <* string ">")
  case mrest of
    Left _  -> openPos `failingWith'` NoClosingCallbackTag oname
    Right (ch, closePos, cname) -> if oname == cname
       then pure ch
       else closePos `failingWith'` BadClosingCallbackTag oname cname
    where children n = do
            eom <- asks endOfInput
            nodes <- nodesTill (lookAhead $ void (string "</") <|> eom)
            pure . Callback n $ nodes
          closing = fmap isJust . hidden . optional . char $ '/'

interp :: Parser (Node -> Node)
interp = between (char '{') (char '}') $ do
  n <- arg
  option (String n) (sep *> body n)
  where sep = string "," <* hspace1
        body n = choice
          [ uncurry (Bool n) <$> (string "boolean" *> sep *> boolCases)
          , Number n <$ string "number"
          , Date n <$> (string "date" *> sep *> dateTimeFmt)
          , Time n <$> (string "time" *> sep *> dateTimeFmt)
          , withPluralCtx n $ choice
              [ string "plural"        *> sep *> cardinalCases n
              , string "selectordinal" *> sep *> ordinalCases n
              ]
          , string "select" *> sep *> selectCases n
          ]
        withPluralCtx n = withReaderT (\x -> x { pluralCtxName = Just n })

dateTimeFmt :: Parser DateTimeFmt
dateTimeFmt = choice
  [ Short  <$ string "short"
  , Medium <$ string "medium"
  , Long   <$ string "long"
  , Full   <$ string "full"
  ]

caseBody :: Parser Node
caseBody = string "{" *> nodesTill (string "}")

boolCases :: Parser (Node, Node)
boolCases = (,)
  <$> (string "true"  *> hspace1 *> caseBody)
   <* hspace1
  <*> (string "false" *> hspace1 *> caseBody)

selectCases :: Arg -> Parser (Node -> Node)
selectCases n = choice
  [ reconcile <$> cases <*> optional wildcard
  , SelectWild n <$> wildcard
  ]
  where cases = NE.sepEndBy1 ((,) <$> (name <* hspace1) <*> caseBody) hspace1
        wildcard = string wildcardName *> hspace1 *> caseBody
        reconcile cs (Just w) = SelectNamedWild n cs w
        reconcile cs Nothing  = SelectNamed n cs
        name = try $ mfilter (/= wildcardName) ident
        wildcardName = "other"

cardinalCases :: Arg -> Parser (Node -> Node)
cardinalCases n = try (cardinalInexactCases n) <|> cardinalExactCases n

cardinalExactCases :: Arg -> Parser (Node -> Node)
cardinalExactCases n = CardinalExact n <$> NE.sepEndBy1 pluralExactCase hspace1

cardinalInexactCases :: Arg -> Parser (Node -> Node)
cardinalInexactCases n = uncurry f <$> mixedPluralCases <*> pluralWildcard
  where f e r w = CardinalInexact n e r w

ordinalCases :: Arg -> Parser (Node -> Node)
ordinalCases n = uncurry f <$> mixedPluralCases <*> pluralWildcard
  where f e r w = Ordinal n e r w

mixedPluralCases :: Parser ([PluralCase PluralExact], [PluralCase PluralRule])
mixedPluralCases = partitionEithers <$> sepEndBy (eitherP pluralExactCase pluralRuleCase) hspace1

pluralExactCase :: Parser (PluralCase PluralExact)
pluralExactCase = (,) <$> pluralExact <* hspace1 <*> caseBody
  where pluralExact = PluralExact . T.pack <$> (string "=" *> some numberChar)

pluralRuleCase :: Parser (PluralCase PluralRule)
pluralRuleCase = (,) <$> pluralRule <* hspace1 <*> caseBody

pluralRule :: Parser PluralRule
pluralRule = choice
  [ Zero <$ string "zero"
  , One  <$ string "one"
  , Two  <$ string "two"
  , Few  <$ string "few"
  , Many <$ string "many"
  ]

pluralWildcard :: Parser Node
pluralWildcard = string "other" *> hspace1 *> caseBody
