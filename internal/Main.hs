module Main where

import           CLI                (Opts (..), getOpts)
import qualified Data.Map           as M
import qualified Data.Text          as T
import           Intlc.Core
import           Intlc.Linter
import           Intlc.Parser       (parseDataset, printErr)
import           Intlc.Parser.Error (ParseFailure)
import           Prelude            hiding (filter)

main :: IO ()
main = getOpts >>= \case
  Lint path -> either parserDie lint =<< getParsed path
  where
    getParsed :: FilePath -> IO (Either ParseFailure (Dataset Translation))
    getParsed x = parseDataset x <$> readFileText x

    parserDie = die . printErr

    lint :: Dataset Translation -> IO ()
    lint = exit . M.mapMaybe (statusToMaybe . lintInternal . message)

    exit :: Dataset (NonEmpty InternalLint) -> IO ()
    exit sts
      | M.size sts > 0 = die . T.unpack . ("Errors\n" <>) . M.foldrWithKey mkLine mempty $ sts
      | otherwise = pure ()

    mkLine :: Text -> NonEmpty InternalLint -> Text -> Text
    mkLine k es acc = acc <> "\n" <> k <> ": " <> e
      where e = T.intercalate ", " . toList . fmap show $ es
