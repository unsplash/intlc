module Main where

import           CLI                (Opts (..), getOpts)
import qualified Data.Map           as M
import qualified Data.Text          as T
import           Intlc.Compiler     (compileDataset, compileFlattened)
import           Intlc.Core
import           Intlc.Linter
import           Intlc.Parser       (parseDataset, printErr)
import           Intlc.Parser.Error (ParseFailure)
import           Prelude

main :: IO ()
main = getOpts >>= \case
  Compile path loc -> tryCompile loc =<< getParsed path
  Flatten path     -> either parserDie (putTextLn . compileFlattened) =<< getParsed path
  Lint    path     -> either parserDie lint =<< getParsed path
    where lint = exit . M.mapMaybe (statusToMaybe . lintExternal . message)
          exit sts
            | M.size sts > 0 = die . T.unpack . ("Errors\n" <>) . M.foldrWithKey mkLine mempty $ sts
            | otherwise = pure ()
          mkLine k es acc = acc <> "\n" <> k <> ": " <> e
            where e = T.intercalate ", " . toList . fmap show $ es
  where tryCompile l = either parserDie (either compilerDie putTextLn . compileDataset l)
        parserDie = die . printErr
        compilerDie = die . T.unpack . ("Invalid keys:\n" <>) . T.intercalate "\n" . fmap ("\t" <>) . toList

getParsed :: FilePath -> IO (Either ParseFailure (Dataset Translation))
getParsed x = parseDataset x <$> readFileText x
