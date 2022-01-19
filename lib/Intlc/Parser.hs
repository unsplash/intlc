-- This module follows the following whitespace rules:
--   * Consume all whitespace after tokens where possible.
--   * Therefore, assume no whitespace before tokens.

module Intlc.Parser where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import           Data.Aeson                               (decode)
import           Data.ByteString.Lazy                     (ByteString)
import qualified Data.Map                                 as M
import qualified Data.Text                                as T
import           Data.Void                                ()
import           Intlc.Core
import           Intlc.ICU
import           Prelude                                  hiding (ByteString)
import           Text.Megaparsec                          hiding (Stream, Token,
                                                           many, some, token)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer               as L
import           Text.Megaparsec.Error.Builder

data ParseFailure
  = FailedJsonParse
  | FailedMessageParse ParseErr
  deriving (Show, Eq)

data MessageParseErr
  = NoClosingCallbackTag Text
  | BadClosingCallbackTag Text Text
  deriving (Show, Eq, Ord)

instance ShowErrorComponent MessageParseErr where
  showErrorComponent (NoClosingCallbackTag x)    = "Callback tag <" <> T.unpack x <> "> not closed"
  showErrorComponent (BadClosingCallbackTag x y) = "Callback tag <" <> T.unpack x <> "> not closed, instead found </" <> T.unpack y <> ">"

printErr :: ParseFailure -> String
printErr FailedJsonParse        = "Failed to parse JSON"
printErr (FailedMessageParse e) = errorBundlePretty e

parseDataset :: ByteString -> Either ParseFailure (Dataset Translation)
parseDataset = parse' <=< decode'
  where decode' = maybeToRight FailedJsonParse . decode
        parse' = M.traverseWithKey ((first FailedMessageParse .) . parseTranslationFor)

parseTranslationFor :: Text -> UnparsedTranslation -> Either ParseErr Translation
parseTranslationFor name (UnparsedTranslation umsg be) =
  flip Translation be <$> parse msg (T.unpack name) umsg

type ParseErr = ParseErrorBundle Text MessageParseErr

type Parser = Parsec MessageParseErr Text

-- | Plaintext is parsed as individual chars. Here we'll merge any siblings.
reconcile :: Stream -> Stream
reconcile []                               = []
reconcile (Plaintext x : Plaintext y : zs) = reconcile $ Plaintext (x <> y) : zs
reconcile (x:ys)                           = x : reconcile ys

ident :: Parser Text
ident = T.pack <$> some letterChar

msg :: Parser Message
msg = f . reconcile <$> manyTill token eof
  where f []            = Static ""
        f [Plaintext x] = Static x
        f (x:xs)        = Dynamic (x :| xs)

token :: Parser Token
token = choice
  [ Interpolation           <$> interp
  , Plaintext . T.singleton <$> L.charLiteral
  ]

callback :: Parser Arg
callback = do
  oname <- string "<" *> ident <* ">"
  mrest <- observing ((,,) <$> children <* string "</" <*> getOffset <*> ident <* string ">")
  case mrest of
    Left _  -> e 1 (NoClosingCallbackTag oname)
    Right (ch, pos, cname) -> if oname == cname
       then pure (Arg oname ch)
       else e pos (BadClosingCallbackTag oname cname)
    where children = Callback . reconcile <$> manyTill token (lookAhead $ string "</")
          e pos = parseError . errFancy pos . fancy . ErrorCustom

interp :: Parser Arg
interp = choice
  [ try $ do
      n <- string "{" *> ident
      Arg n <$> body n <* string "}"
  , callback
  ]
  where sep = string "," <* hspace1
        body n = option String $ sep *> choice
          [ Number <$ string "number"
          , Date <$> (string "date" *> sep *> dateFmt)
          , Plural <$> (string "plural" *> sep *> pluralCases n)
          , uncurry Select <$> (string "select" *> sep *> selectCases)
          ]

dateFmt :: Parser DateFmt
dateFmt = choice
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
        body = reconcile <$> (string "{" *> manyTill token (string "}"))
        wildcardName = "other"

pluralCases :: Text -> Parser Plural
pluralCases name = (tryClassify =<<) $ (,) <$> plurals <*> optional wildcard
  where body = reconcile <$> (string "{" *> manyTill token' (string "}"))
          -- Plural cases support interpolating the number in context with `#`.
          where token' = Interpolation (Arg name Number) <$ string "#" <|> token
        plurals = flip NE.sepEndBy1 hspace1 $ choice
          [ (ParsedExact .) . PluralCase <$> numId <* hspace1 <*> body
          , (ParsedRule .)  . PluralCase <$> rule  <* hspace1 <*> body
          ]
          where numId = PluralExact . T.pack <$> (string "=" *> some numberChar)
                rule = choice
                  [ Zero <$ string "zero"
                  , One  <$ string "one"
                  , Two  <$ string "two"
                  , Few  <$ string "few"
                  , Many <$ string "many"
                  ]
        wildcard = PluralWildcard <$> (wildcardMatch *> hspace1 *> body)
        wildcardMatch = string "other"
        tryClassify = maybe empty pure . classifyPluralCases

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
classifyPluralCases :: Foldable f => (f ParsedPluralCase, Maybe PluralWildcard) -> Maybe Plural
classifyPluralCases (xs, mw) =
  case (organise xs, mw) of
    ((Just ls, Nothing), mw')     -> Just (LitPlural   ls mw')
    ((Nothing, Just rs), Just w)  -> Just (RulePlural  rs w)
    ((Just ls, Just rs), Just w)  -> Just (MixedPlural ls rs w)
    -- Rule plurals require a wildcard.
    ((_,       Just _),  Nothing) -> Nothing
    -- We should have parsed and organised at least one case somewhere.
    ((Nothing, Nothing), _)       -> Nothing
  where organise :: Foldable f => f ParsedPluralCase -> (Maybe (NonEmpty (PluralCase PluralExact)), Maybe (NonEmpty (PluralCase PluralRule)))
        organise = bimap nonEmpty nonEmpty . foldr f mempty
        f (ParsedExact x) = first (x:)
        f (ParsedRule x)  = second (x:)

-- Need to lift parsed cases into this type to make the list homogeneous.
data ParsedPluralCase
  = ParsedExact (PluralCase PluralExact)
  | ParsedRule (PluralCase PluralRule)
