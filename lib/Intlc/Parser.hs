module Intlc.Parser where

import qualified Data.Text             as T
import           Intlc.Core
import           Intlc.Parser.ICU      (MessageParseErr, initialState, msg)
import           Intlc.Parser.JSON     (dataset)
import           Prelude               hiding (ByteString)
import           Text.Megaparsec       (runParser)
import           Text.Megaparsec.Error

parseDataset :: FilePath -> Text -> Either ParseFailure (Dataset Translation)
parseDataset name contents = first (FailedDatasetParse . pure) parsed
  where parsed = runParser dataset name contents

parseTranslationFor :: Text -> UnparsedTranslation -> Either ParseErr Translation
parseTranslationFor name (UnparsedTranslation umsg be md) = do
  msg' <- runParser (runReaderT msg initialState) (T.unpack name) umsg
  pure $ Translation msg' be md

type ParseErr = ParseErrorBundle Text MessageParseErr

data ParseFailure
  = FailedJsonParse
  | FailedDatasetParse (NonEmpty ParseErr)
  deriving (Show, Eq)

printErr :: ParseFailure -> String
printErr FailedJsonParse         = "Failed to parse JSON"
printErr (FailedDatasetParse es) = intercalate "\n" . toList . fmap errorBundlePretty $ es
