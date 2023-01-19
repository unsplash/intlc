module Main where

import           CLI                (ICUModifiers (..), Opts (..), getOpts)
import qualified Data.Text          as T
import           Intlc.Compiler     (compileDataset, compileToJSON,
                                     expandPlurals, flatten)
import           Intlc.Core
import           Intlc.ICU          (AnnNode, Message, Node, sansAnn)
import           Intlc.Linter
import           Intlc.Parser       (parseDataset, parseMessage, printErr)
import           Intlc.Parser.Error (ParseFailure)
import           Intlc.Prettify     (prettify)
import           Prelude

main :: IO ()
main = getOpts >>= \case
  Compile path loc   -> tryGetParsedAtSansAnn path >>= compile loc
  Flatten path fo ms -> tryGetParsedAtSansAnn path >>= (compileToJSON f fo >>> putTextLn)
    -- Beware that not all transformations can safely fuse. For example
    -- flattening must run against the entire AST by itself to necessarily be
    -- coherent.
    --
    -- Beware also a simple `mconcat` against `mods` without the clarifying
    -- `Endo`!
    where f = appEndo (mconcat (Endo <$> mods)) . flatten
          mods = ms <&> \case
            ExpandPlurals -> expandPlurals
  Lint    path lr    -> lint lr path
  Prettify msg       -> tryPrettify msg

compile :: MonadIO m => Locale -> Dataset (Translation (Message Node)) -> m ()
compile loc = compileDataset loc >>> \case
  Left es -> die . T.unpack . ("Invalid keys:\n" <>) . T.intercalate "\n" . fmap ("\t" <>) . toList $ es
  Right x -> putTextLn x

lint :: MonadIO m => LintRuleset -> FilePath -> m ()
lint lr path = do
  raw <- readFileAt path
  dataset <- parserDie $ parseDataset path raw
  whenJust (lintDataset lr path raw dataset) $ die . T.unpack

tryPrettify :: MonadIO m => Text -> m ()
tryPrettify = either (die . printErr) (putTextLn . prettify . fmap sansAnn) . parseMessage "input"

tryGetParsedAtSansAnn :: MonadIO m => FilePath -> m (Dataset (Translation (Message Node)))
tryGetParsedAtSansAnn = parserDie . fmap datasetSansAnn <=< getParsedAt

tryGetParsedAt :: MonadIO m => FilePath -> m (Dataset (Translation (Message AnnNode)))
tryGetParsedAt = parserDie <=< getParsedAt

parserDie :: MonadIO m => Either ParseFailure a -> m a
parserDie = either (die . printErr) pure

getParsedAt :: MonadIO m => FilePath -> m (Either ParseFailure (Dataset (Translation (Message AnnNode))))
getParsedAt x = parseDataset x <$> readFileAt x

readFileAt :: MonadIO m => FilePath -> m Text
readFileAt = fmap decodeUtf8 . readFileBS
