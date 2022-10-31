module Main where

import           CLI                         (Opts (..), getOpts)
import qualified Data.Text                   as T
import           Data.Text.IO                (getContents)
import           Intlc.Backend.JSON.Compiler (compileDataset)
import           Intlc.Compiler              (expandPlurals)
import           Intlc.Core
import           Intlc.Linter
import           Intlc.Parser                (parseDataset, printErr)
import           Intlc.Parser.Error          (ParseFailure)
import           Prelude                     hiding (filter)

main :: IO ()
main = getOpts >>= \case
  Lint path     -> tryGetParsedAtSansAnn path >>= lint
  ExpandPlurals -> tryGetParsedStdinSansAnn >>= compileExpandedPlurals

lint :: MonadIO m => Dataset Translation -> m ()
lint xs = whenJust (lintDatasetInternal xs) $ die . T.unpack

compileExpandedPlurals :: MonadIO m => Dataset Translation -> m ()
compileExpandedPlurals = putTextLn . compileDataset . fmap (\x -> x { message = expandPlurals x.message })

tryGetParsedStdinSansAnn :: IO (Dataset Translation)
tryGetParsedStdinSansAnn = parserDie . fmap datasetSansAnn =<< getParsedStdin

tryGetParsedStdin :: IO (Dataset AnnTranslation)
tryGetParsedStdin = parserDie =<< getParsedStdin

tryGetParsedAtSansAnn :: MonadIO m => FilePath -> m (Dataset Translation)
tryGetParsedAtSansAnn = parserDie . fmap datasetSansAnn <=< getParsedAt

tryGetParsedAt :: MonadIO m => FilePath -> m (Dataset AnnTranslation)
tryGetParsedAt = parserDie <=< getParsedAt

parserDie :: MonadIO m => Either ParseFailure a -> m a
parserDie = either (die . printErr) pure

getParsedStdin :: IO (Either ParseFailure (Dataset AnnTranslation))
getParsedStdin = parseDataset "stdin" <$> getContents

getParsedAt :: MonadIO m => FilePath -> m (Either ParseFailure (Dataset AnnTranslation))
getParsedAt x = parseDataset x . decodeUtf8 <$> readFileBS x
