module Main where

import           CLI                         (Opts (..), getOpts)
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           Data.Text.IO                (getContents)
import           Intlc.Backend.JSON.Compiler (compileDataset)
import           Intlc.Compiler              (expandPlurals)
import           Intlc.Core
import           Intlc.Linter
import           Intlc.Parser                (parseDataset, printErr)
import           Intlc.Parser.Error          (ParseFailure)
import           Prelude                     hiding (filter)

main :: IO ()
main = getOpts >>= \case
  Lint path     -> either parserDie lint' =<< getParsed path
  ExpandPlurals -> either parserDie expandPlurals' . parseDataset "stdin" =<< getContents
  where
    getParsed :: FilePath -> IO (Either ParseFailure (Dataset Translation))
    getParsed x = parseDataset x <$> readFileText x

    parserDie = die . printErr

    expandPlurals' :: Dataset Translation -> IO ()
    expandPlurals' = putTextLn . compileDataset . fmap (\x -> x { message = expandPlurals (message x) })

    lint' :: Dataset Translation -> IO ()
    lint' = exit . M.mapMaybe (statusToMaybe . lint . message)

    exit :: Dataset (NonEmpty LintingError) -> IO ()
    exit sts
      | M.size sts > 0 = die . T.unpack . ("Errors\n" <>) . M.foldrWithKey mkLine mempty $ sts
      | otherwise = pure ()

    mkLine :: Text -> NonEmpty LintingError -> Text -> Text
    mkLine k es acc = acc <> "\n" <> k <> ": " <> e
      where e = T.intercalate ", " . toList . fmap show $ es
