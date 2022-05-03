-- An in-house JSON parser specialised to our needs, piggybacking off of the
-- sibling ICU parser. Allows interop with our ICU parser and bypasses some
-- Aeson limitations.
--
-- This module follows the following whitespace rules:
--   * Consume all whitespace after tokens where possible.
--   * Therefore, assume no whitespace before tokens.

{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Intlc.Parser.JSON where

import           Control.Applicative.Permutations
import qualified Data.Text                        as T
import           Data.Void                        ()
import           Intlc.Core
import qualified Intlc.ICU                        as ICU
import           Intlc.Parser.Error               (JSONParseErr (..),
                                                   ParseErr (FailedJSONParse),
                                                   failingWith)
import           Intlc.Parser.ICU                 (initialState, toMsg, token)
import           Prelude                          hiding (null)
import           Text.Megaparsec                  hiding (State, Stream, Token,
                                                   many, some, token)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L
import           Utils                            (toNubMap)

type Parser = Parsec ParseErr Text

failingWith' :: MonadParsec ParseErr s m => Int -> JSONParseErr -> m a
i `failingWith'` e = i `failingWith` FailedJSONParse e

dataset :: Parser (Dataset Translation)
dataset = space *> objMap translation <* space <* eof

-- It's important to use `toPermutationWithDefault` as opposed to standard
-- parser combinators like `optional` so that `intercalateEffect` can do its
-- magic.
--
-- Additionally, the consistent application of whitespace is extremely
-- important, and the permutation appears to operate over the first parser, so
-- be careful around any abstractions around the key double quotes.
translation :: Parser Translation
translation = obj $ intercalateEffect objSep $ Translation
  <$> toPermutation                       (objPair' "message"     msg)
  <*> toPermutationWithDefault TypeScript (objPair' "backend"     (backendp <|> TypeScript <$ null))
  <*> toPermutationWithDefault Nothing    (objPair' "description" (Just <$> strLit <|> Nothing <$ null))

msg :: Parser ICU.Message
msg = withRecovery recover p
  where p = toMsg <$> runReaderT (char '"' *> manyTill token (char '"')) initialState
        recover e = error "absurd" <$ consume <* registerParseError e
        -- Once we've recovered we need to consume the rest of the message
        -- string so that parsing can continue beyond it.
        consume = void $ manyTill L.charLiteral (char '"')

backendp :: Parser Backend
backendp = choice
  [ TypeScript      <$ string (dblqts "ts")
  , TypeScriptReact <$ string (dblqts "tsx")
  ]

null :: Parser ()
null = void $ string "null"

strLit :: Parser Text
strLit = (T.pack <$>) $ char '"' *> manyTill L.charLiteral (char '"')

dblqtsp :: Parser a -> Parser a
dblqtsp = between (char '"') (char '"')

dblqts :: Text -> Text
dblqts x = "\"" <> x <> "\""

-- Parse a homogeneous object of arbitrary keys, failing upon the presence of
-- duplicate keys.
objMap :: Parser a -> Parser (Map Text a)
objMap v = obj $ toNubMap' =<< sepEndBy (objPair strLit v) objSep
  where toNubMap' = toNubMap (failingWith' 1 . DuplicateKeys)

obj :: Parser a -> Parser a
obj p = string "{" *> space *> p <* space <* string "}"

objPair :: Parser Text -> Parser a -> Parser (Text, a)
objPair k v = (,) <$> k <*> (space *> char ':' *> space *> v)

objPair' :: Text -> Parser a -> Parser a
objPair' k v = snd <$> objPair (string (dblqts k)) v

objSep :: Parser ()
objSep = void $ char ',' <* space
