module Main where

import           CLI       (Opts (..), getOpts)
import qualified Data.Text as T
import           Prelude

main :: IO ()
main = getOpts >>= \case
  Lint x -> putTextLn $ "Hello, " <> T.pack x <> "!"
