-- Our parsers are unavoidably tied together, and it's easiest if dependent
-- parsers share the same error type, but we also need to avoid cyclic
-- dependencies - so it all lives here.

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Intlc.Parser.Error where

import qualified Data.Text                     as T
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
  = DuplicateKeys (NonEmpty Text)
  deriving (Show, Eq, Ord)

data MessageParseErr
  = NoClosingCallbackTag Text
  | BadClosingCallbackTag Text Text
  deriving (Show, Eq, Ord)

instance ShowErrorComponent ParseErr where
  showErrorComponent (FailedJSONParse e) = showErrorComponent e
  showErrorComponent (FailedMsgParse e)  = showErrorComponent e

instance ShowErrorComponent JSONParseErr where
  showErrorComponent (DuplicateKeys xs) = "Duplicate keys: " <> T.unpack (T.intercalate ", " (toList xs))

instance ShowErrorComponent MessageParseErr where
  showErrorComponent (NoClosingCallbackTag x)    = "Callback tag <" <> T.unpack x <> "> not closed"
  showErrorComponent (BadClosingCallbackTag x y) = "Callback tag <" <> T.unpack x <> "> not closed, instead found </" <> T.unpack y <> ">"

failingWith :: MonadParsec e s m => Int -> e -> m a
pos `failingWith` e = parseError . errFancy pos . fancy . ErrorCustom $ e
