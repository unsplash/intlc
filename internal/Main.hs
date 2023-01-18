module Main where

import           CLI                         (Opts (..), getOpts)
import           Data.Text.IO                (getContents)
import qualified Intlc.Backend.JSON.Compiler as JSON
import           Intlc.Compiler              (expandPlurals)
import           Intlc.Core
import           Intlc.ICU                   (AnnNode, Message, Node)
import           Intlc.Parser                (parseDataset, printErr)
import           Intlc.Parser.Error          (ParseFailure)
import           Prelude                     hiding (filter)

main :: IO ()
main = getOpts >>= \case
  ExpandPlurals fo -> tryGetParsedStdinSansAnn >>= compileExpandedPlurals fo

compileExpandedPlurals :: MonadIO m => JSON.Formatting -> Dataset (Translation (Message Node)) -> m ()
compileExpandedPlurals fo = putTextLn . JSON.compileDataset fo . fmap f
  where f x = x { message = expandPlurals x.message }

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
