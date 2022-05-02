module Intlc.Parser where

import           Data.Aeson            (decode)
import           Data.ByteString.Lazy  (ByteString)
import qualified Data.Map              as M
import qualified Data.Text             as T
import           Data.Validation       (toEither, validationNel)
import           Intlc.Core
import           Intlc.Parser.ICU      (MessageParseErr, initialState, msg)
import           Prelude               hiding (ByteString)
import           Text.Megaparsec       (runParser)
import           Text.Megaparsec.Error

parseDataset :: ByteString -> Either ParseFailure (Dataset Translation)
parseDataset = parse' <=< decode'
  where decode' = maybeToRight FailedJsonParse . decode
        parse' = toEither . first FailedDatasetParse . M.traverseWithKey ((validationNel .) . parseTranslationFor)

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
