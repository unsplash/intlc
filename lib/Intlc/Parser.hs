module Intlc.Parser where

import           Intlc.Core
import           Intlc.Parser.Error    (ParseFailure)
import           Intlc.Parser.JSON     (dataset)
import           Prelude
import           Text.Megaparsec       (runParser)
import           Text.Megaparsec.Error

parseDataset :: FilePath -> Text -> Either ParseFailure (Dataset Translation)
parseDataset = runParser dataset

printErr :: ParseFailure -> String
printErr = errorBundlePretty
