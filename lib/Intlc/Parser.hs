{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Intlc.Parser where

import           Intlc.Core
import           Intlc.Parser.ICU      (MessageParseErr, initialState, msg)
import           Intlc.Parser.JSON     (dataset)
import           Prelude
import           Text.Megaparsec       (runParser)
import           Text.Megaparsec.Error

parseDataset :: FilePath -> Text -> Either ParseFailure (Dataset Translation)
parseDataset name contents = first FailedMsgParse parsed
  where parsed = runParser dataset name contents

type ParseErr = ParseErrorBundle Text MessageParseErr

data ParseFailure
  = FailedMsgParse ParseErr
  deriving (Show, Eq)

printErr :: ParseFailure -> String
printErr (FailedMsgParse e) = errorBundlePretty e
