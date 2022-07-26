module Main where

import           CLI                (Opts (..), getOpts)
import qualified Data.Text          as T
import           Intlc.Compiler     (compileDataset, compileFlattened)
import           Intlc.Core
import           Intlc.Linter
import           Intlc.Parser       (parseDataset, printErr)
import           Intlc.Parser.Error (ParseFailure)
import           Prelude

main :: IO ()
main = getOpts >>= \case
  Compile path loc -> tryGetParsedAt path >>= compile loc
  Flatten path     -> tryGetParsedAt path >>= flatten
  Lint    path     -> tryGetParsedAt path >>= lint

compile :: MonadIO m => Locale -> Dataset Translation -> m ()
compile loc = compileDataset loc >>> \case
  Left es -> die . T.unpack . ("Invalid keys:\n" <>) . T.intercalate "\n" . fmap ("\t" <>) . toList $ es
  Right x -> putTextLn x

flatten :: MonadIO m => Dataset Translation -> m ()
flatten = putTextLn . compileFlattened

lint :: MonadIO m => Dataset Translation -> m ()
lint xs = whenJust (lintDatasetExternal xs) $ die . T.unpack

tryGetParsedAt :: MonadIO m => FilePath -> m (Dataset Translation)
tryGetParsedAt = parserDie <=< getParsedAt

parserDie :: MonadIO m => Either ParseFailure a -> m a
parserDie = either (die . printErr) pure

getParsedAt :: MonadIO m => FilePath -> m (Either ParseFailure (Dataset Translation))
getParsedAt x = parseDataset x . decodeUtf8 <$> readFileBS x
