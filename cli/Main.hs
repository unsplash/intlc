module Main where

import           Data.ByteString.Lazy (getContents)
import qualified Data.Text            as T
import           Intlc.Compiler       (dataset)
import           Intlc.Parser         (parseDataset, printErr)
import           Prelude              hiding (stdin)

main :: IO ()
main = handleParse . parseDataset =<< getContents
  where handleParse = either parserDie (handleValidation . dataset)
        parserDie = die . printErr
        handleValidation = either validationDie putTextLn
        validationDie = die . T.unpack . T.intercalate "\n" . toList
