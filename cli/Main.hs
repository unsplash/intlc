module Main where

import           CLI            (Opts (..), getOpts)
import qualified Data.Text      as T
import           Intlc.Compiler (dataset)
import           Intlc.Parser   (parseDataset, printErr)
import           Prelude

main :: IO ()
main = do
  opts <- getOpts
  contents <- readFileLBS (path opts)
  handleParse . parseDataset $ contents
  where handleParse = either parserDie (handleValidation . dataset)
        parserDie = die . printErr
        handleValidation = either validationDie putTextLn
        validationDie = die . T.unpack . T.intercalate "\n" . toList
