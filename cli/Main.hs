module Main where

import           Intlc.Compiler (translation)
import           Intlc.Parser   (parseTranslationFor)
import           Prelude

main :: IO ()
main = fin . parseTranslationFor "stdin" =<< getLine
  where fin = either (die . show) (putTextLn . translation)
