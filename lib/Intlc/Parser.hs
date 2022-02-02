-- This module follows the following whitespace rules:
--   * Consume all whitespace after tokens where possible.
--   * Therefore, assume no whitespace before tokens.

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Intlc.Parser where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import           Data.Aeson                               (decode)
import           Data.ByteString.Lazy                     (ByteString)
import           Data.Char                                (isAlpha)
import qualified Data.Map                                 as M
import qualified Data.Text                                as T
import           Data.Void                                ()
import           Intlc.Core
import           Intlc.ICU
import           Prelude                                  hiding (ByteString)
import           Text.Megaparsec                          hiding (State, Stream,
                                                           Token, many, some,
                                                           token)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer               as L
import           Text.Megaparsec.Error.Builder

data ParseFailure
  = FailedJsonParse
  | InvalidKeys (NonEmpty Text)
  | FailedMessageParse ParseErr
  deriving (Show, Eq)

data MessageParseErr
  = NoClosingCallbackTag Text
  | BadClosingCallbackTag Text Text
  deriving (Show, Eq, Ord)

instance ShowErrorComponent MessageParseErr where
  showErrorComponent (NoClosingCallbackTag x)    = "Callback tag <" <> T.unpack x <> "> not closed"
  showErrorComponent (BadClosingCallbackTag x y) = "Callback tag <" <> T.unpack x <> "> not closed, instead found </" <> T.unpack y <> ">"

failingWith :: MonadParsec e s m => Int -> e -> m a
pos `failingWith` e = parseError . errFancy pos . fancy . ErrorCustom $ e

printErr :: ParseFailure -> String
printErr FailedJsonParse        = "Failed to parse JSON"
printErr (InvalidKeys ks)       = T.unpack $ "Invalid keys: " <> T.intercalate ", " (toList ks)
printErr (FailedMessageParse e) = errorBundlePretty e

parseDataset :: ByteString -> Either ParseFailure (Dataset Translation)
parseDataset = parse' <=< validateKeys <=< decode'
  where decode' = maybeToRight FailedJsonParse . decode
        parse' = M.traverseWithKey ((first FailedMessageParse .) . parseTranslationFor)

validateKeys :: Dataset a -> Either ParseFailure (Dataset a)
validateKeys xs = toEither . nonEmpty . filter (not . isValidKey) . M.keys $ xs
  where toEither Nothing   = Right xs
        toEither (Just ks) = Left . InvalidKeys $ ks
        isValidKey = T.all (liftA2 (||) isAlpha (== '_'))

parseTranslationFor :: Text -> UnparsedTranslation -> Either ParseErr Translation
parseTranslationFor name (UnparsedTranslation umsg be) =
  flip Translation be <$> evalState (runParserT msg (T.unpack name) umsg) initialState

type ParseErr = ParseErrorBundle Text MessageParseErr

data ParserState = ParserState
  -- FIFO. The head is the most recent addition to the stack.
  { pluralArgNameStack :: [Text]
  }

initialState :: ParserState
initialState = ParserState mempty

peekPluralArgName :: ParserState -> Maybe Text
peekPluralArgName = listToMaybe . pluralArgNameStack

pushPluralArgName :: Text -> ParserState -> ParserState
pushPluralArgName n x = x { pluralArgNameStack = n : pluralArgNameStack x }

popPluralArgName :: ParserState -> (Maybe Text, ParserState)
popPluralArgName x =
  case pluralArgNameStack x of
    []     -> (Nothing, x)
    (y:ys) -> (Just y, x { pluralArgNameStack = ys })

popPluralArgName_ :: ParserState -> ParserState
popPluralArgName_ = snd . popPluralArgName

type Parser = ParsecT MessageParseErr Text (State ParserState)

ident :: Parser Text
ident = T.pack <$> some letterChar

msg :: Parser Message
msg = f . mergePlaintext <$> manyTill token eof
  where f []            = Static ""
        f [Plaintext x] = Static x
        f (x:xs)        = Dynamic (x :| xs)

token :: Parser Token
token = do
  marg <- gets peekPluralArgName
  choice
    [ Interpolation <$> (interp <|> callback)
    -- Plural cases support interpolating the number/argument in context with
    -- `#`. When there's no such context, fail the parse in effect treating it
    -- as plaintext.
    , case marg of
        Just n  -> Interpolation (Arg n Number) <$ string "#"
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

callback :: Parser Arg
callback = do
  oname <- string "<" *> ident <* ">"
  mrest <- observing ((,,) <$> children <* string "</" <*> getOffset <*> ident <* string ">")
  case mrest of
    Left _  -> 1 `failingWith` NoClosingCallbackTag oname
    Right (ch, pos, cname) -> if oname == cname
       then pure (Arg oname ch)
       else pos `failingWith` BadClosingCallbackTag oname cname
    where children = Callback . mergePlaintext <$> manyTill token (lookAhead $ string "</")

interp :: Parser Arg
interp = do
  n <- string "{" *> ident
  Arg n <$> choice
    [ String <$ string "}"
    , sep *> body n <* string "}"
    ]
  where sep = string "," <* hspace1
        body n = choice
          [ Number <$ string "number"
          , Date <$> (string "date" *> sep *> dateTimeFmt)
          , Time <$> (string "time" *> sep *> dateTimeFmt)
          , Plural <$> withPluralCtx n (string "plural" *> sep *> cardinalPluralCases)
          , Plural <$> withPluralCtx n (string "selectordinal" *> sep *> ordinalPluralCases)
          , uncurry Select <$> (string "select" *> sep *> selectCases)
          ]
        withPluralCtx n p = pushPluralCtx n *> p <* popPluralCtx
        pushPluralCtx = modify . pushPluralArgName
        popPluralCtx = modify popPluralArgName_

dateTimeFmt :: Parser DateTimeFmt
dateTimeFmt = choice
  [ Short  <$ string "short"
  , Medium <$ string "medium"
  , Long   <$ string "long"
  , Full   <$ string "full"
  ]

selectCases :: Parser (NonEmpty SelectCase, Maybe SelectWildcard)
selectCases = (,) <$> cases <*> optional wildcard
  where cases = NE.sepEndBy1 (SelectCase <$> (name <* hspace1) <*> body) hspace1
        wildcard = SelectWildcard <$> (string wildcardName *> hspace1 *> body)
        name = try $ mfilter (/= wildcardName) ident
        body = mergePlaintext <$> (string "{" *> manyTill token (string "}"))
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
  [ (ParsedExact .) . PluralCase <$> pluralExact <* hspace1 <*> pluralBody
  , (ParsedRule .)  . PluralCase <$> pluralRule  <* hspace1 <*> pluralBody
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
pluralWildcard = PluralWildcard <$> (string "other" *> hspace1 *> pluralBody)

pluralBody :: Parser Stream
pluralBody = mergePlaintext <$> (string "{" *> manyTill token (string "}"))

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
