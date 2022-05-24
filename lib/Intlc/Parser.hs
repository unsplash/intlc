module Intlc.Parser where

import           Intlc.Core
import           Intlc.Parser.Error    (ParseFailure)
import           Intlc.Parser.JSON     (ParserState (ParserState), dataset)
import           Prelude
import           Text.Megaparsec       (runParser)
import           Text.Megaparsec.Error

parseDataset :: FilePath -> Text -> Either ParseFailure (Dataset Translation)
parseDataset = runParser (evalStateT dataset (ParserState mempty))

printErr :: ParseFailure -> String
printErr = errorBundlePretty
