module Intlc.Parser where

import qualified Data.Text                  as T
import           Data.Void                  ()
import           Intlc.Core
import           Prelude
import           Text.Megaparsec            hiding (Token, many, some, token)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type ParseOutput = Either (ParseErrorBundle Text Void) Translation

type Parser = Parsec Void Text

parseTranslationFor :: Text -> Text -> ParseOutput
parseTranslationFor = parse translation . T.unpack

translation :: Parser Translation
translation = manyTill token eof

token :: Parser Token
token = choice
  [ Interpolation <$> interp
  , Plaintext     <$> text
  ]

sep :: Parser ()
sep = void $ string "%%"

text :: Parser Text
text = T.pack <$> manyTill L.charLiteral (lookAhead $ sep <|> eof)

interp :: Parser Text
interp = T.pack <$> (sep *> manyTill L.charLiteral sep)
