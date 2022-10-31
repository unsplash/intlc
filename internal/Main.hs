module Main where

import           CLI                         (Opts (..), getOpts)
import qualified Data.Text                   as T
import           Data.Text.IO                (getContents)
import           Intlc.Backend.JSON.Compiler (compileDataset)
import           Intlc.Compiler              (expandPlurals)
import           Intlc.Core
import           Intlc.ICU                   (AnnMessage, Message)
import           Intlc.Linter
import           Intlc.Parser                (parseDataset, printErr)
import           Intlc.Parser.Error          (ParseFailure)
import           Prelude                     hiding (filter)

main :: IO ()
main = getOpts >>= \case
  Lint path     -> tryGetParsedAt path >>= lint
  ExpandPlurals -> tryGetParsedStdinSansAnn >>= compileExpandedPlurals

lint :: MonadIO m => Dataset (Translation AnnMessage) -> m ()
lint xs = whenJust (lintDatasetInternal xs) $ die . T.unpack

compileExpandedPlurals :: MonadIO m => Dataset (Translation Message) -> m ()
compileExpandedPlurals = putTextLn . compileDataset . fmap (\x -> x { message = expandPlurals x.message })

tryGetParsedStdinSansAnn :: IO (Dataset (Translation Message))
tryGetParsedStdinSansAnn = parserDie . fmap datasetSansAnn =<< getParsedStdin

tryGetParsedStdin :: IO (Dataset (Translation AnnMessage))
tryGetParsedStdin = parserDie =<< getParsedStdin

tryGetParsedAt :: MonadIO m => FilePath -> m (Dataset (Translation AnnMessage))
tryGetParsedAt = parserDie <=< getParsedAt

parserDie :: MonadIO m => Either ParseFailure a -> m a
parserDie = either (die . printErr) pure

getParsedStdin :: IO (Either ParseFailure (Dataset (Translation AnnMessage)))
getParsedStdin = parseDataset "stdin" <$> getContents

getParsedAt :: MonadIO m => FilePath -> m (Either ParseFailure (Dataset (Translation AnnMessage)))
getParsedAt x = parseDataset x . decodeUtf8 <$> readFileBS x
