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
  Lint path     -> either parserDie lint' =<< getParsed path
  ExpandPlurals -> either parserDie expandPlurals' . parseDataset "stdin" =<< getContents
  where
    getParsed :: FilePath -> IO (Either ParseFailure (Dataset Translation))
    getParsed x = parseDataset x <$> readFileText x

    parserDie = die . printErr

    expandPlurals' :: Dataset Translation -> IO ()
    expandPlurals' = putTextLn . compileDataset . fmap (\x -> x { message = expandPlurals (message x) })

    lint' :: Dataset Translation -> IO ()
    lint' = exit . M.mapMaybe (statusToMaybe . lintInternal . message)

    exit :: Dataset (NonEmpty InternalLint) -> IO ()
    exit sts
      | M.size sts > 0 = mapM_ printLine (M.assocs sts)
      | otherwise      = pure ()

    printLine :: (Text, NonEmpty InternalLint) -> IO ()
    printLine (k, es) = putTextLn $ title <> msgs
      where title = k <> ": \n"
            msgs = T.intercalate "\n" . toList . fmap (indent . formatLintingError) $ es
            indent = (" " <>)
