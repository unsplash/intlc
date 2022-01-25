module Main where

import           CLI            (Opts (..), getOpts)
import qualified Data.Text      as T
import           Intlc.Compiler (compileDataset, compileFlattened)
import           Intlc.Core
import           Intlc.Parser   (ParseFailure, parseDataset, printErr)
import           Prelude

main :: IO ()
main = getOpts >>= \case
  Compile path loc -> tryCompile loc =<< getParsed path
  Flatten path -> either parserDie (putLBSLn . compileFlattened) =<< getParsed path
  where tryCompile l = either parserDie (handleValidation . compileDataset l)
        parserDie = die . printErr
        handleValidation = either validationDie putTextLn
        validationDie = die . T.unpack . T.intercalate "\n" . toList

getParsed :: FilePath -> IO (Either ParseFailure (Dataset Translation))
getParsed = fmap parseDataset . readFileLBS

