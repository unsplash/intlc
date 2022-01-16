{-# LANGUAGE FlexibleContexts #-}

module Intlc.Parser where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import           Data.Aeson                               (decode)
import           Data.ByteString.Lazy                     (ByteString)
import qualified Data.Map                                 as M
import qualified Data.Text                                as T
import           Data.Void                                ()
import           Intlc.Core
import           Prelude                                  hiding (ByteString)
import           Text.Megaparsec                          hiding (Token, many,
                                                           some, token)
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
reconcile :: [Token] -> [Token]
reconcile []                               = []
reconcile (Plaintext x : Plaintext y : zs) = reconcile $ Plaintext (x <> y) : zs
reconcile (x:ys)                           = x : reconcile ys

msg :: Parser Message
msg = f . reconcile <$> manyTill token eof
  where f []            = Static ""
        f [Plaintext x] = Static x
        f xs            = Dynamic xs

token :: Parser Token
token = choice
  [ Interpolation           <$> interp
  , Plaintext . T.singleton <$> L.charLiteral
  ]

callback :: Parser Arg
callback = do
  oname <- string "<" *> namep <* ">"
  mrest <- observing ((,,) <$> children <* string "</" <*> getOffset <*> namep <* string ">")
  case mrest of
    Left _  -> e 1 (NoClosingCallbackTag oname)
    Right (ch, pos, cname) -> if oname == cname
       then pure (Arg oname ch)
       else e pos (BadClosingCallbackTag oname cname)
    where namep = T.pack <$> manyTill L.charLiteral (lookAhead $ string ">")
          children = Callback . reconcile <$> manyTill token (lookAhead $ string "</")
          e pos = parseError . errFancy pos . fancy . ErrorCustom

interp :: Parser Arg
interp = choice
  [ try $ do
      n <- string "{" *> name
      Arg n <$> body n <* string "}"
  , callback
  ]
  where sep = string ", "
        name = T.pack <$> some letterChar
        body n = option String $ sep *> choice
          [ Number <$ string "number"
          , Date <$> (string "date" *> sep *> dateFmt)
          , uncurry Plural <$> (string "plural" *> sep *> pluralCases n)
          ]

dateFmt :: Parser DateFmt
dateFmt = choice
  [ Short  <$ string "short"
  , Medium <$ string "medium"
  , Long   <$ string "long"
  , Full   <$ string "full"
  ]

pluralCases :: Text -> Parser (NonEmpty PluralCase, PluralWildcard)
pluralCases name = (,) <$> plurals <*> wildcard
  where body = reconcile <$> (string "{" *> manyTill token' (string "}"))
          -- Plural cases support interpolating the number in context with `#`.
          where token' = (Interpolation (Arg name Number) <$ string "#") <|> token
        plurals = NE.someTill (PluralCase <$> numId <*> body <* string " ") (lookAhead $ string "o")
          where numId = T.pack <$> (string "=" *> some numberChar <* string " ")
        wildcard = PluralWildcard <$> (string "other " *> body)
