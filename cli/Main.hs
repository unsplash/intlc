module Main where

import           Data.ByteString.Lazy (getContents)
import           Intlc.Compiler       (dataset)
import           Intlc.Parser         (parseDataset)
import           Prelude              hiding (stdin)

main :: IO ()
main = fin . parseDataset =<< getContents
  where fin = either (die . show) (putTextLn . dataset)
