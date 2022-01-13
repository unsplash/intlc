module Main where

import           Data.ByteString.Lazy (getContents)
import           Intlc.Compiler       (dataset)
import           Intlc.Parser         (parseDataset, printErr)
import           Prelude              hiding (stdin)

main :: IO ()
main = fin . parseDataset =<< getContents
  where fin = either (die . printErr) (putTextLn . dataset)
