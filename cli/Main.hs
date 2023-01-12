module Main where

import           CLI                         (Opts (..), getOpts)
import qualified Data.Text                   as T
import qualified Intlc.Backend.JSON.Compiler as JSON
import           Intlc.Compiler              (compileDataset, compileFlattened)
import           Intlc.Core
import           Intlc.ICU                   (AnnNode, Message, Node, sansAnn)
import           Intlc.Linter
import           Intlc.Parser                (parseDataset, parseMessage,
                                              printErr)
import           Intlc.Parser.Error          (ParseFailure)
import           Intlc.Prettify              (prettify)
import           Prelude

main :: IO ()
main = getOpts >>= \case
  Compile path loc -> tryGetParsedAtSansAnn path >>= compile loc
  Flatten path fo  -> tryGetParsedAtSansAnn path >>= flatten fo
  Lint    path     -> lint path
  Prettify msg     -> tryPrettify msg

compile :: MonadIO m => Locale -> Dataset (Translation (Message Node)) -> m ()
compile loc = compileDataset loc >>> \case
  Left es -> die . T.unpack . ("Invalid keys:\n" <>) . T.intercalate "\n" . fmap ("\t" <>) . toList $ es
  Right x -> putTextLn x

flatten :: MonadIO m => JSON.Formatting -> Dataset (Translation (Message Node)) -> m ()
flatten fo = putTextLn . compileFlattened fo

lint :: MonadIO m => FilePath -> m ()
lint path = do
  raw <- readFileAt path
  dataset <- parserDie $ parseDataset path raw
  whenJust (lintDatasetExternal path raw dataset) $ die . T.unpack

tryPrettify :: MonadIO m => Text -> m ()
tryPrettify = either (die . printErr) (putTextLn . prettify . fmap sansAnn) . parseMessage "input"

tryGetParsedAtSansAnn :: MonadIO m => FilePath -> m (Dataset (Translation (Message Node)))
tryGetParsedAtSansAnn = parserDie . fmap datasetSansAnn <=< getParsedAt

tryGetParsedAt :: MonadIO m => FilePath -> m (Dataset (Translation (Message AnnNode)))
tryGetParsedAt = parserDie <=< getParsedAt

parserDie :: MonadIO m => Either ParseFailure a -> m a
parserDie = either (die . printErr) pure

getParsedAt :: MonadIO m => FilePath -> m (Either ParseFailure (Dataset (Translation (Message AnnNode))))
getParsedAt x = parseDataset x <$> readFileAt x

readFileAt :: MonadIO m => FilePath -> m Text
readFileAt = fmap decodeUtf8 . readFileBS
