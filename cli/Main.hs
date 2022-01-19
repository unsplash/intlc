module Main where

import           CLI            (Opts (..), getOpts)
import qualified Data.Text      as T
import           Intlc.Compiler (compileDataset)
import           Intlc.Parser   (parseDataset, printErr)
import           Prelude

main :: IO ()
main = do
  opts <- getOpts
  contents <- readFileLBS (path opts)
  handleParse (locale opts) . parseDataset $ contents
  where handleParse l = either parserDie (handleValidation . compileDataset l)
        parserDie = die . printErr
        handleValidation = either validationDie putTextLn
        validationDie = die . T.unpack . T.intercalate "\n" . toList
