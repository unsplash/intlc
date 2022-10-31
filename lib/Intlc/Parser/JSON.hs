-- An in-house JSON parser specialised to our needs, piggybacking off of the
-- sibling ICU parser. Allows interop with our ICU parser and bypasses some
-- Aeson limitations.
--
-- This module follows the following whitespace rules:
--   * Consume all whitespace after nodes where possible.
--   * Therefore, assume no whitespace before nodes.

module Intlc.Parser.JSON where

import           Control.Applicative.Permutations
import qualified Data.Map                         as M
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Data.Void                        ()
import           Intlc.Core
import qualified Intlc.ICU                        as ICU
import           Intlc.Parser.Error               (JSONParseErr (..),
                                                   ParseErr (FailedJSONParse),
                                                   failingWith)
import qualified Intlc.Parser.ICU                 as ICUP
import           Prelude                          hiding (null)
import           Text.Megaparsec                  hiding (State, Stream, many,
                                                   some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L
import           Text.Megaparsec.Error.Builder    (errFancy, fancy)

type Parser = StateT ParserState (Parsec ParseErr Text)

data ParserState = ParserState
  { keys :: Set Text
  }

failingWith' :: MonadParsec ParseErr s m => Int -> JSONParseErr -> m a
i `failingWith'` e = i `failingWith` FailedJSONParse e

dataset :: Parser (Dataset (Translation ICU.AnnMessage))
dataset = space *> objMap translation <* space <* eof

-- It's important to use `toPermutationWithDefault` as opposed to standard
-- parser combinators like `optional` so that `intercalateEffect` can do its
-- magic.
--
-- Additionally, the consistent application of whitespace is extremely
-- important, and the permutation appears to operate over the first parser, so
-- be careful around any abstractions around the key double quotes.
translation :: Parser (Translation ICU.AnnMessage)
translation = obj $ intercalateEffect objSep $ Translation
  <$> toPermutation                       (objPair' "message"     msg)
  <*> toPermutationWithDefault TypeScript (objPair' "backend"     (backendp <|> TypeScript <$ null))
  <*> toPermutationWithDefault Nothing    (objPair' "description" (Just <$> strLit <|> Nothing <$ null))

msg :: Parser ICU.AnnMessage
msg = lift $ withRecovery recover p
  where p = runReaderT (char '"' *> ICUP.annMsg) icupState
        icupState = ICUP.emptyState { ICUP.endOfInput = void $ char '"' }
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

-- Parse a homogeneous object of arbitrary keys, failing with recovery upon the
-- presence of duplicate keys.
objMap :: Parser a -> Parser (Map Text a)
objMap v = fmap M.fromList . obj $ sepEndBy (objPair newKey v) objSep
  where newKey = do
          i <- getOffset
          k <- strLit
          prev <- gets keys
          if Set.member k prev
             then registerParseError . errFancy i . fancy . ErrorCustom . FailedJSONParse . DuplicateKey $ k
             else modify (\x -> x { keys = Set.insert k prev })
          pure k

obj :: Parser a -> Parser a
obj p = string "{" *> space *> p <* space <* string "}"

objPair :: Parser Text -> Parser a -> Parser (Text, a)
objPair k v = (,) <$> k <*> (space *> char ':' *> space *> v)

objPair' :: Text -> Parser a -> Parser a
objPair' k v = snd <$> objPair (string (dblqts k)) v

objSep :: Parser ()
objSep = void $ char ',' <* space
