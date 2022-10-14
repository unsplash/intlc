-- Our parsers are unavoidably tied together, and it's easiest if dependent
-- parsers share the same error type, but we also need to avoid cyclic
-- dependencies - so it all lives here.

module Intlc.Parser.Error where

import qualified Data.Text                     as T
import           Intlc.ICU                     (Arg, unArg)
import           Prelude
import           Text.Megaparsec               (MonadParsec (parseError))
import           Text.Megaparsec.Error
import           Text.Megaparsec.Error.Builder

type ParseFailure = ParseErrorBundle Text ParseErr

data ParseErr
  = FailedJSONParse JSONParseErr
  | FailedMsgParse MessageParseErr
  deriving (Show, Eq, Ord)

data JSONParseErr
  = DuplicateKey Text
  deriving (Show, Eq, Ord)

data MessageParseErr
  = NoClosingCallbackTag Arg
  | BadClosingCallbackTag Arg Arg
  | NoOpeningCallbackTag Arg
  deriving (Show, Eq, Ord)

instance ShowErrorComponent ParseErr where
  showErrorComponent (FailedJSONParse e) = showErrorComponent e
  showErrorComponent (FailedMsgParse e)  = showErrorComponent e

instance ShowErrorComponent JSONParseErr where
  showErrorComponent (DuplicateKey k) = "Duplicate key: \"" <> T.unpack k <> "\""

instance ShowErrorComponent MessageParseErr where
  showErrorComponent (NoClosingCallbackTag x)    = "Callback tag <" <> T.unpack (unArg x) <> "> not closed"
  showErrorComponent (BadClosingCallbackTag x y) = "Callback tag <" <> T.unpack (unArg x) <> "> not closed, instead found </" <> T.unpack (unArg y) <> ">"
  showErrorComponent (NoOpeningCallbackTag x)    = "Callback tag </" <> T.unpack (unArg x) <> "> not opened"

failingWith :: MonadParsec e s m => Int -> e -> m a
pos `failingWith` e = parseError . errFancy pos . fancy . ErrorCustom $ e
