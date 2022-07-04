-- This module follows the following whitespace rules:
--   * Consume all whitespace after tokens where possible.
--   * Therefore, assume no whitespace before tokens.

{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Intlc.Parser.ICU where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import qualified Data.Text                                as T
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

initialState :: ParserState
initialState = ParserState
  { pluralCtxName = Nothing
  , endOfInput = pure ()
  }

type Parser = ReaderT ParserState (Parsec ParseErr Text)

ident :: Parser Text
ident = T.pack <$> some letterChar

-- Parse a message until the end of input parser matches.
msg :: Parser Message
msg = msgTill =<< asks endOfInput

-- Parse a message until the provided parser matches.
msgTill :: Parser a -> Parser Message
msgTill = fmap (Message . mergePlaintext) . streamTill

-- Parse a stream until the provided parser matches.
streamTill :: Parser a -> Parser [Token]
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
  , Plaintext <$> (try escaped <|> plaintext)
  ]

plaintext :: Parser Text
plaintext = T.singleton <$> L.charLiteral

escaped :: Parser Text
escaped = apos *> choice
  -- Double escape two apostrophes as one: "''" -> "'"
  [ "'" <$ apos
  -- Escape everything until another apostrophe, being careful of internal
  -- double escapes: "'{a''}'" -> "{a'}"
  , try $ T.pack <$> someTillNotDouble L.charLiteral apos
  -- Escape the next syntax character as plaintext: "'{" -> "{"
  , T.singleton <$> syn
  ]
  where apos = char '\''
        syn = char '{' <|> char '<'
        -- Like `someTill`, but doesn't end upon encountering two `end` tokens,
        -- instead consuming them as one and continuing.
        someTillNotDouble p end = tryOne
          where tryOne = (:) <$> p <*> go
                go = ((:) <$> try (end <* end) <*> go) <|> (mempty <$ end) <|> tryOne

callback :: Parser (Text, Type)
callback = do
  (openPos, oname) <- (,) <$> (string "<" *> getOffset) <*> ident <* string ">"
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
          , uncurry Select <$> (string "select" *> sep *> selectCases)
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

selectCases :: Parser (NonEmpty SelectCase, Maybe SelectWildcard)
selectCases = (,) <$> cases <*> optional wildcard
  where cases = NE.sepEndBy1 (SelectCase <$> (name <* hspace1) <*> caseBody) hspace1
        wildcard = SelectWildcard <$> (string wildcardName *> hspace1 *> caseBody)
        name = try $ mfilter (/= wildcardName) ident
        wildcardName = "other"

cardinalPluralCases :: Parser Plural
cardinalPluralCases = fmap Cardinal . tryClassify =<< p
    where tryClassify = maybe empty pure . uncurry classifyCardinal
          p = (,) <$> disorderedPluralCases <*> optional pluralWildcard

ordinalPluralCases :: Parser Plural
ordinalPluralCases = fmap Ordinal . tryClassify =<< p
    where tryClassify = maybe empty pure . uncurry classifyOrdinal
          p = (,) <$> disorderedPluralCases <*> pluralWildcard

-- Need to lift parsed plural cases into this type to make the list homogeneous.
data ParsedPluralCase
  = ParsedExact (PluralCase PluralExact)
  | ParsedRule (PluralCase PluralRule)

disorderedPluralCases :: Parser (NonEmpty ParsedPluralCase)
disorderedPluralCases = flip NE.sepEndBy1 hspace1 $ choice
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

-- | To simplify parsing cases we validate after-the-fact here. This achieves
-- two purposes. Firstly it enables us to fail the parse if the cases are not
-- exclusively literals and there's no wildcard (see below), and secondly it
-- allows us to organise the cases into the appropriate `Plural` constructors,
-- which in turn enables more efficient codegen later on.
--
--  =0 {}  =1 {}            -- Lit
--  =0 {}  =1 {} other {}   -- Lit
-- one {} two {} other {}   -- Rule
--  =0 {} one {} other {}   -- Mixed
--
classifyCardinal :: Foldable f => f ParsedPluralCase -> Maybe PluralWildcard -> Maybe CardinalPlural
classifyCardinal xs mw =
  case (organisePluralCases xs, mw) of
    ((Just ls, Nothing), mw')     -> Just (LitPlural   ls mw')
    ((Nothing, Just rs), Just w)  -> Just (RulePlural  rs w)
    ((Just ls, Just rs), Just w)  -> Just (MixedPlural ls rs w)
    -- Rule plurals require a wildcard.
    ((_,       Just _),  Nothing) -> Nothing
    -- We should have parsed and organised at least one case somewhere.
    ((Nothing, Nothing), _)       -> Nothing

-- | This is simpler than its cardinal counterpart. Here we need only validate
-- that there is at least one rule case. This is performed here to simplify
-- supporting disordered cases in the parser (whereas validating the presence
-- of a wildcard at the end is trivial in the parser).
classifyOrdinal :: Foldable f => f ParsedPluralCase -> PluralWildcard -> Maybe OrdinalPlural
classifyOrdinal xs w =
  case organisePluralCases xs of
    (_, Nothing)   -> Nothing
    (mls, Just rs) -> Just $ OrdinalPlural (foldMap toList mls) rs w

organisePluralCases :: Foldable f => f ParsedPluralCase -> (Maybe (NonEmpty (PluralCase PluralExact)), Maybe (NonEmpty (PluralCase PluralRule)))
organisePluralCases = bimap nonEmpty nonEmpty . foldr f mempty
  where f (ParsedExact x) = first (x:)
        f (ParsedRule x)  = second (x:)
