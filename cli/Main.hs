module Main where

import           CLI                (Opts (..), getOpts)
import qualified Data.Text          as T
import           Intlc.Compiler     (compileDataset, compileFlattened)
import           Intlc.Core
import           Intlc.Parser       (parseDataset, printErr)
import           Intlc.Parser.Error (ParseFailure)
import           Prelude

main :: IO ()
main = getOpts >>= \case
  Compile path loc -> tryCompile loc =<< getParsed path
  Flatten path -> either parserDie (putTextLn . compileFlattened) =<< getParsed path
  where tryCompile l = either parserDie (either compilerDie putTextLn . compileDataset l)
        parserDie = die . printErr
        compilerDie = die . T.unpack . ("Invalid keys:\n" <>) . T.intercalate "\n" . fmap ("\t" <>) . toList

getParsed :: FilePath -> IO (Either ParseFailure (Dataset Translation))
getParsed x = parseDataset x <$> readFileText x
