module Main where

import           CLI                (Opts (..), getOpts)
import qualified Data.Text          as T
import           Intlc.Compiler     (compileDataset, compileFlattened)
import           Intlc.Core
import           Intlc.ICU          (AnnMessage, Message, sansAnnMsg)
import           Intlc.Linter
import           Intlc.Parser       (parseDataset, parseMessage, printErr)
import           Intlc.Parser.Error (ParseFailure)
import           Intlc.Prettify     (prettify)
import           Prelude

main :: IO ()
main = getOpts >>= \case
  Compile path loc -> tryGetParsedAtSansAnn path >>= compile loc
  Flatten path     -> tryGetParsedAtSansAnn path >>= flatten
  Lint    path     -> tryGetParsedAtSansAnn path >>= lint
  Prettify msg     -> tryPrettify msg

compile :: MonadIO m => Locale -> Dataset (Translation Message) -> m ()
compile loc = compileDataset loc >>> \case
  Left es -> die . T.unpack . ("Invalid keys:\n" <>) . T.intercalate "\n" . fmap ("\t" <>) . toList $ es
  Right x -> putTextLn x

flatten :: MonadIO m => Dataset (Translation Message) -> m ()
flatten = putTextLn . compileFlattened

lint :: MonadIO m => Dataset (Translation Message) -> m ()
lint xs = whenJust (lintDatasetExternal xs) $ die . T.unpack

tryPrettify :: MonadIO m => Text -> m ()
tryPrettify = either (die . printErr) (putTextLn . prettify . sansAnnMsg) . parseMessage "input"

tryGetParsedAtSansAnn :: MonadIO m => FilePath -> m (Dataset (Translation Message))
tryGetParsedAtSansAnn = parserDie . fmap datasetSansAnn <=< getParsedAt

tryGetParsedAt :: MonadIO m => FilePath -> m (Dataset (Translation AnnMessage))
tryGetParsedAt = parserDie <=< getParsedAt

parserDie :: MonadIO m => Either ParseFailure a -> m a
parserDie = either (die . printErr) pure

getParsedAt :: MonadIO m => FilePath -> m (Either ParseFailure (Dataset (Translation AnnMessage)))
getParsedAt x = parseDataset x . decodeUtf8 <$> readFileBS x
