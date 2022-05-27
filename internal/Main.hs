module Main where

import           CLI                (Opts (..), getOpts)
import           Data.Map           (filter, foldrWithKey, size)
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
    lint' = exit . filter isFailure . fmap (lint . message)

    exit :: Dataset Status -> IO ()
    exit sts
      | size sts > 0 = (die . ("Errors\n" <>) . T.unpack . foldrWithKey mkLine mempty) sts
      | otherwise = putLTextLn "Success"

    mkLine :: Text -> Status -> Text -> Text
    mkLine k s acc = acc <> "\n" <> k <> ": " <> show s

