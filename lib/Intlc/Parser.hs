module Intlc.Parser where

import qualified Data.Text             as T
import           Intlc.Core
import qualified Intlc.ICU             as ICU
import           Intlc.Parser.Error    (ParseFailure)
import           Intlc.Parser.ICU      (msg')
import           Intlc.Parser.JSON     (ParserState (ParserState), dataset)
import           Prelude
import           Text.Megaparsec       (runParser)
import           Text.Megaparsec.Error

parseDataset :: FilePath -> Text -> Either ParseFailure (Dataset Translation)
parseDataset = runParser (evalStateT dataset (ParserState mempty))

parseMessage :: Text -> Text -> Either ParseFailure ICU.Message
parseMessage src = runParser msg' (T.unpack src)

printErr :: ParseFailure -> String
printErr = errorBundlePretty
