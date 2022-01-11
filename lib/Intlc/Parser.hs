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

-- | Plaintext is parsed as individual chars. Here we'll merge any siblings.
reconcile :: [Token] -> [Token]
reconcile []                               = []
reconcile (Plaintext x : Plaintext y : zs) = reconcile $ Plaintext (x <> y) : zs
reconcile (x:ys)                           = x : reconcile ys

translation :: Parser Translation
translation = f . reconcile <$> manyTill token eof
  where f []            = Static ""
        f [Plaintext x] = Static x
        f xs            = Dynamic xs

token :: Parser Token
token = choice
  [ Interpolation . Arg     <$> interp
  , Plaintext . T.singleton <$> L.charLiteral
  ]

interp :: Parser Text
interp = T.pack <$> (string "{" *> manyTill L.charLiteral (string "}"))
