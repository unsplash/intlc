module Main where

import           CLI                         (Opts (..), getOpts)
import qualified Data.Text                   as T
import           Data.Text.IO                (getContents)
import           Intlc.Backend.JSON.Compiler (compileDataset)
import           Intlc.Compiler              (expandPlurals)
import           Intlc.Core
import           Intlc.ICU                   (AnnNode, Message, Node)
import           Intlc.Linter
import           Intlc.Parser                (parseDataset, printErr)
import           Intlc.Parser.Error          (ParseFailure)
import           Prelude                     hiding (filter)

main :: IO ()
main = getOpts >>= \case
  Lint path     -> lint path
  ExpandPlurals -> tryGetParsedStdinSansAnn >>= compileExpandedPlurals

lint :: MonadIO m => FilePath -> m ()
lint path = do
  raw <- readFileAt path
  dataset <- parserDie $ parseDataset path raw
  whenJust (lintDatasetInternal path raw dataset) $ die . T.unpack

compileExpandedPlurals :: MonadIO m => Dataset (Translation (Message Node)) -> m ()
compileExpandedPlurals = putTextLn . compileDataset . fmap (\x -> x { message = expandPlurals x.message })

tryGetParsedStdinSansAnn :: IO (Dataset (Translation (Message Node)))
tryGetParsedStdinSansAnn = parserDie . fmap datasetSansAnn =<< getParsedStdin

tryGetParsedStdin :: IO (Dataset (Translation (Message AnnNode)))
tryGetParsedStdin = parserDie =<< getParsedStdin

parserDie :: MonadIO m => Either ParseFailure a -> m a
parserDie = either (die . printErr) pure

getParsedStdin :: IO (Either ParseFailure (Dataset (Translation (Message AnnNode))))
getParsedStdin = parseDataset "stdin" <$> getContents

readFileAt :: MonadIO m => FilePath -> m Text
readFileAt = fmap decodeUtf8 . readFileBS
