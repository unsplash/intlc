module Main where

import           CLI                (Opts (..), getOpts)
import           Intlc.Core
import           Intlc.Linter
import           Intlc.Parser       (parseDataset, printErr)
import           Intlc.Parser.Error (ParseFailure)
import           Prelude


main :: IO ()
main = getOpts >>= \case
  Lint path -> either parserDie (mkStatus . go) =<< getParsed path
  where
    getParsed :: FilePath -> IO (Either ParseFailure (Dataset Translation))
    getParsed x = parseDataset x <$> readFileText x

    parserDie = die . printErr

    -- TODO we could short-circuit to the first error or accumulate errors
    go :: Dataset Translation -> Status
    go = foldr (\a b -> lint (message a)) Success

    mkStatus :: Status -> IO ()
    mkStatus = \case
      Success   -> putTextLn (show Success)
      Failure x -> die (show x)


