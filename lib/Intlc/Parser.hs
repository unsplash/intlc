{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Intlc.Parser where

import qualified Data.Text             as T
import           Intlc.Core
import           Intlc.Parser.ICU      (MessageParseErr, initialState, msg)
import           Intlc.Parser.JSON     (dataset)
import           Prelude
import           Text.Megaparsec       (runParser)
import           Text.Megaparsec.Error

parseDataset :: FilePath -> Text -> Either ParseFailure (Dataset Translation)
parseDataset name contents = first FailedMsgParse parsed
  where parsed = runParser dataset name contents

parseTranslationFor :: Text -> UnparsedTranslation -> Either ParseErr Translation
parseTranslationFor name (UnparsedTranslation umsg be md) = do
  msg' <- runParser (runReaderT msg initialState) (T.unpack name) umsg
  pure $ Translation msg' be md

type ParseErr = ParseErrorBundle Text MessageParseErr

data ParseFailure
  = FailedMsgParse ParseErr
  deriving (Show, Eq)

printErr :: ParseFailure -> String
printErr (FailedMsgParse e) = errorBundlePretty e
