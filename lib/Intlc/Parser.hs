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
  | FailedMessageParse ParseErr
  deriving (Show, Eq)

parseDataset :: ByteString -> Either ParseFailure (Dataset Translation)
parseDataset = parse' <=< decode'
  where decode' = maybeToRight FailedJsonParse . decode
        parse' = M.traverseWithKey ((first FailedMessageParse .) . parseTranslationFor)

parseTranslationFor :: Text -> UnparsedTranslation -> Either ParseErr Translation
parseTranslationFor name (UnparsedTranslation umsg be) =
  flip Translation be <$> parse msg (T.unpack name) umsg

type ParseErr = ParseErrorBundle Text Void

type Parser = Parsec Void Text

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
  [ Interpolation           <$> try interp
  , Plaintext . T.singleton <$> L.charLiteral
  ]

callback :: Parser Arg
callback = do
  name <- string "<" *> namep <* ">"
  Arg name <$> children <* string "</" <* string name <* string ">"
    where namep = T.pack <$> manyTill L.charLiteral (lookAhead $ string ">")
          children = Just . Callback <$> manyTill token (lookAhead $ string "</")

interp :: Parser Arg
interp = choice
  [ Arg <$> (string "{" *> name) <*> optional (sep *> num) <* string "}"
  , callback
  ]
  where name = T.pack <$> manyTill L.charLiteral (lookAhead $ string "," <|> string "}")
        sep = void $ string ", "
        num = Number <$ string "number"
