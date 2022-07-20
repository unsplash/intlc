module Main where

import           CLI                         (Opts (..), getOpts)
import qualified Data.Map                    as M
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
  Lint path     -> tryGetParsedAt path >>= lint
  ExpandPlurals -> tryGetParsedStdin >>= compileExpandedPlurals

lint :: MonadIO m => Dataset Translation -> m ()
lint xs = do
  let lints = M.mapMaybe (statusToMaybe . lintInternal . message) xs
  let msg = T.intercalate "\n" $ uncurry formatInternalFailure <$> M.assocs lints
  unless (M.null lints) $ die (T.unpack msg)

compileExpandedPlurals :: MonadIO m => Dataset Translation -> m ()
compileExpandedPlurals = putTextLn . compileDataset . fmap (\x -> x { message = expandPlurals (message x) })

tryGetParsedStdin :: IO (Dataset Translation)
tryGetParsedStdin = parserDie =<< getParsedStdin

tryGetParsedAt :: MonadIO m => FilePath -> m (Dataset Translation)
tryGetParsedAt = parserDie <=< getParsedAt

parserDie :: MonadIO m => Either ParseFailure a -> m a
parserDie = either (die . printErr) pure

getParsedStdin :: IO (Either ParseFailure (Dataset Translation))
getParsedStdin = parseDataset "stdin" <$> getContents

getParsedAt :: MonadIO m => FilePath -> m (Either ParseFailure (Dataset Translation))
getParsedAt x = parseDataset x <$> readFileText x
