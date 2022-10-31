-- Parse an ICU message with annotations/source offsets. These annotations can
-- be stripped later externally if unwanted.
--
-- Most parsers parse for functions. This might seem counterintuitive, but it's
-- to match the shape of our AST in which, like a singly-linked list, each node
-- points to its next sibling. Equivalently a `Parser (a -> NodeF a)` is
-- typically a parser which has parsed all but the following sibling. A simple
-- example of this would be `pure (CharF 'a')`.
--
-- This module follows the following whitespace rules:
--   * Consume all whitespace after nodes where possible.
--   * Therefore, assume no whitespace before nodes.

module Intlc.Parser.ICU where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import           Control.Comonad.Cofree                   (Cofree ((:<)),
                                                           unwrap)
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

-- | Lifts the contents of the parser to contain annotations at this layer of
-- the `Cofree`.
withAnn :: Parser (AnnNode -> NodeF AnnNode) -> Parser (AnnNode -> AnnNode)
withAnn p = (\i f z -> i :< f z) <$> getOffset <*> p

ident :: Parser Text
ident = label "alphabetic identifier" $ T.pack <$> some letterChar

arg :: Parser Arg
arg = Arg <$> ident

-- | Parse a message with annotations until end of input.
--
-- To instead parse a message as part of a broader data structure, instead look
-- at `msg` and its `endOfInput` state property.
annMsg' :: Parsec ParseErr Text AnnMessage
annMsg' = runReaderT annMsg cfg where
  cfg = emptyState { endOfInput = eof }

-- Parse a message with annotations until the end of input parser matches.
annMsg :: Parser AnnMessage
annMsg = annMsgTill =<< asks endOfInput

-- Parse a message with annotations until the provided parser matches.
annMsgTill :: Parser a -> Parser AnnMessage
annMsgTill = fmap AnnMessage . nodesTill

nodesTill :: Parser a -> Parser AnnNode
nodesTill end = go where
  go = fin <|> (withAnn node <*> go)
  fin = (:< FinF) <$> (getOffset <* end)

-- The core parser of this module. Parse as many of these as you'd like until
-- reaching an anticipated delimiter, such as a double quote in the surrounding
-- JSON string or end of input in a REPL.
node :: Parser (AnnNode -> NodeF AnnNode)
node = choice
  [ interp
  , callback
  -- Plural cases support interpolating the number/argument in context with
  -- `#`. When there's no such context, fail the parse in effect treating it
  -- as plaintext.
  , asks pluralCtxName >>= \case
      Just n  -> PluralRefF n <$ string "#"
      Nothing -> empty
  , plaintext
  ]

-- Parse a character or a potentially larger escape sequence.
plaintext :: Parser (AnnNode -> NodeF AnnNode)
plaintext = choice
  [ try escaped
  , CharF <$> L.charLiteral
  ]

-- Follows ICU 4.8+ spec, see:
--   https://unicode-org.github.io/icu/userguide/format_parse/messages/#quotingescaping
escaped :: Parser (AnnNode -> NodeF AnnNode)
escaped = apos *> choice
  -- Double escape two apostrophes as one, regardless of surrounding
  -- syntax: "''" -> "'"
  [ CharF <$> apos
  -- Escape everything until another apostrophe, being careful of internal
  -- double escapes: "'{a''}'" -> "{a'}". Must ensure it doesn't surpass the
  -- bounds of the surrounding parser as per `endOfInput`.
  , try $ do
      eom <- asks endOfInput
      head' <- withAnn (CharF <$> synOpen)
      -- Try and parse until end of input or a lone apostrophe. If end of input
      -- comes first then fail the parse.
      (tail', wasEom) <- someTill_ (withAnn plaintext) $ choice
        [       True  <$ eom
        , try $ False <$ apos <* notFollowedBy apos
        ]
      guard (not wasEom)
      pure $ unwrap . foldr (.) id (head' : tail')
  -- Escape the next syntax character as plaintext: "'{" -> "{"
  , CharF <$> synAll
  ]
  where apos = char '\''
        synAll = synLone <|> synOpen <|> synClose
        synLone = char '#'
        synOpen = char '{' <|> char '<'
        synClose = char '}' <|> char '>'

callback :: Parser (AnnNode -> NodeF AnnNode)
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
            pure . CallbackF n $ nodes
          closing = fmap isJust . hidden . optional . char $ '/'

interp :: Parser (AnnNode -> NodeF AnnNode)
interp = between (char '{') (char '}') $ do
  n <- arg
  option (StringF n) (sep *> body n)
  where sep = string "," <* hspace1
        body n = choice
          [ uncurry (BoolF n) <$> (string "boolean" *> sep *> boolCases)
          , NumberF n <$ string "number"
          , DateF n <$> (string "date" *> sep *> dateTimeFmt)
          , TimeF n <$> (string "time" *> sep *> dateTimeFmt)
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

caseBody :: Parser AnnNode
caseBody = string "{" *> nodesTill (string "}")

boolCases :: Parser (AnnNode, AnnNode)
boolCases = (,)
  <$> (string "true"  *> hspace1 *> caseBody)
   <* hspace1
  <*> (string "false" *> hspace1 *> caseBody)

selectCases :: Arg -> Parser (AnnNode -> NodeF AnnNode)
selectCases n = choice
  [ reconcile <$> cases <*> optional wildcard
  , SelectWildF n <$> wildcard
  ]
  where cases = NE.sepEndBy1 ((,) <$> (name <* hspace1) <*> caseBody) hspace1
        wildcard = string wildcardName *> hspace1 *> caseBody
        reconcile cs (Just w) = SelectNamedWildF n cs w
        reconcile cs Nothing  = SelectNamedF n cs
        name = try $ mfilter (/= wildcardName) ident
        wildcardName = "other"

cardinalCases :: Arg -> Parser (AnnNode -> NodeF AnnNode)
cardinalCases n = try (cardinalInexactCases n) <|> cardinalExactCases n

cardinalExactCases :: Arg -> Parser (AnnNode -> NodeF AnnNode)
cardinalExactCases n = CardinalExactF n <$> NE.sepEndBy1 pluralExactCase hspace1

cardinalInexactCases :: Arg -> Parser (AnnNode -> NodeF AnnNode)
cardinalInexactCases n = uncurry (CardinalInexactF n) <$> mixedPluralCases <*> pluralWildcard

ordinalCases :: Arg -> Parser (AnnNode -> NodeF AnnNode)
ordinalCases n = uncurry (OrdinalF n) <$> mixedPluralCases <*> pluralWildcard

mixedPluralCases :: Parser ([PluralCaseF PluralExact AnnNode], [PluralCaseF PluralRule AnnNode])
mixedPluralCases = partitionEithers <$> sepEndBy (eitherP pluralExactCase pluralRuleCase) hspace1

pluralExactCase :: Parser (PluralCaseF PluralExact AnnNode)
pluralExactCase = (,) <$> pluralExact <* hspace1 <*> caseBody
  where pluralExact = PluralExact . T.pack <$> (string "=" *> some numberChar)

pluralRuleCase :: Parser (PluralCaseF PluralRule AnnNode)
pluralRuleCase = (,) <$> pluralRule <* hspace1 <*> caseBody

pluralRule :: Parser PluralRule
pluralRule = choice
  [ Zero <$ string "zero"
  , One  <$ string "one"
  , Two  <$ string "two"
  , Few  <$ string "few"
  , Many <$ string "many"
  ]

pluralWildcard :: Parser AnnNode
pluralWildcard = string "other" *> hspace1 *> caseBody
