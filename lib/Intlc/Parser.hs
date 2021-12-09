module Intlc.Parser where

import           Data.Aeson                 (decode)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Void                  ()
import           Intlc.Core
import           Prelude                    hiding (ByteString)
import           Text.Megaparsec            hiding (Token, many, some, token)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data ParseFailure
  = FailedJsonParse
  | FailedTranslationParse ParseErr
  deriving (Show, Eq)

parseDataset :: ByteString -> Either ParseFailure (Dataset Translation)
parseDataset = parse' <=< decode'
  where decode' = maybeToRight FailedJsonParse . decode
        parse' = M.traverseWithKey ((first FailedTranslationParse .) . parseTranslationFor)

type ParseErr = ParseErrorBundle Text Void

type Parser = Parsec Void Text

parseTranslationFor :: Text -> Text -> Either ParseErr Translation
parseTranslationFor = parse translation . T.unpack

translation :: Parser Translation
translation = f <$> manyTill token eof
  where f []            = Static ""
        f [Plaintext x] = Static x
        f xs            = Dynamic xs

token :: Parser Token
token = choice
  [ Interpolation . Arg <$> interp
  , Plaintext           <$> text
  ]

sep :: Parser ()
sep = void $ string "%%"

text :: Parser Text
text = T.pack <$> manyTill L.charLiteral (lookAhead $ sep <|> eof)

interp :: Parser Text
interp = T.pack <$> (sep *> manyTill L.charLiteral sep)
