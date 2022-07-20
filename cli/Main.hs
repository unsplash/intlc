module Main where

import           CLI                (Opts (..), getOpts)
import qualified Data.Map           as M
import qualified Data.Text          as T
import           Intlc.Compiler     (compileDataset, compileFlattened)
import           Intlc.Core
import           Intlc.Linter
import           Intlc.Parser       (parseDataset, printErr)
import           Intlc.Parser.Error (ParseFailure)
import           Prelude
import           System.Exit        (ExitCode (ExitFailure))

main :: IO ()
main = getOpts >>= \case
  Compile path loc -> tryGetParsed path >>= (compileDataset loc >>> either compilerDie putTextLn)
  Flatten path     -> tryGetParsed path >>= (compileFlattened >>> putTextLn)
  Lint    path     -> tryGetParsed path >>= lint
  where compilerDie = die . T.unpack . ("Invalid keys:\n" <>) . T.intercalate "\n" . fmap ("\t" <>) . toList
        lint = exit . M.mapMaybe (statusToMaybe . lintExternal . message)
        exit :: Dataset (NonEmpty ExternalLint) -> IO ()
        exit sts = unless (M.null sts) $ putTextLn msg *> exitWith (ExitFailure 1)
          where msg = T.intercalate "\n" $ uncurry formatExternalFailure <$> M.assocs sts

tryGetParsed :: MonadIO m => FilePath -> m (Dataset Translation)
tryGetParsed = either (die . printErr) pure <=< getParsed

getParsed :: MonadIO m => FilePath -> m (Either ParseFailure (Dataset Translation))
getParsed x = parseDataset x <$> readFileText x
