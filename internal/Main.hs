module Main where

import           CLI                (Opts (..), getOpts)
import qualified Data.Map           as M
import qualified Data.Text          as T
import           Intlc.Core
import           Intlc.Linter
import           Intlc.Parser       (parseDataset, printErr)
import           Intlc.Parser.Error (ParseFailure)
import           Prelude            hiding (filter)


main :: IO ()
main = getOpts >>= \case
  Lint path -> either parserDie lint' =<< getParsed path
  where
    getParsed :: FilePath -> IO (Either ParseFailure (Dataset Translation))
    getParsed x = parseDataset x <$> readFileText x

    parserDie = die . printErr

    lint' :: Dataset Translation -> IO ()
    lint' = exit . M.mapMaybe (statusToMaybe . lint . message)

    exit :: Dataset (NonEmpty LintingError) -> IO ()
    exit sts
      | M.size sts > 0 =   mapM_ printLine (M.assocs sts)
      | otherwise = pure ()


    printLine :: (Text,NonEmpty LintingError) -> IO()
    printLine (k ,es) =  putStrLn (T.unpack k <> ": ") >> tab >> e
      where e = sequence_ . toList . fmap printLintingError $ es
            tab:: IO()
            tab = putStr " "

