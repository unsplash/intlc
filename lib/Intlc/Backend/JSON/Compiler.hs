module Intlc.Backend.JSON.Compiler where

import           Data.List.Extra            (escapeJSON)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Intlc.Backend.ICU.Compiler as ICU
import           Intlc.Core
import           Intlc.ICU                  (Message, Node)
import           Intlc.Printer              (IndentStyle, indenter)
import           Prelude

type Compiler = Reader Config

data Config = Config
  -- Expected to be potentially supplied externally.
  { fmt          :: Formatting
  -- Expected to be supplied internally.
  , indentLevels :: Nat
  }

-- | For prettified formatting we simply indent and inject newlines at objects.
data Formatting
  = Minified
  | Pretty IndentStyle

increment :: Compiler a -> Compiler a
increment = local $ \x -> x { indentLevels = x.indentLevels + 1 }

-- Assumes unescaped input.
dblqts :: Text -> Text
dblqts v = "\"" <> escapeJSONText v <> "\""
  where escapeJSONText = T.pack . escapeJSON . T.unpack

strVal :: Text -> Text
strVal = dblqts

nullVal :: Text
nullVal = "null"

objKey :: Text -> Text
objKey = dblqts

-- | This is where we'll manage indentation for all objects, hence taking a
-- monadic input.
obj :: Compiler [(Text, Text)] -> Compiler Text
obj xs = asks fmt >>= \case
  Minified -> do
    let objPair k v = objKey k <> ":" <> v
    contents <- T.intercalate "," . fmap (uncurry objPair) <$> xs
    pure $ "{" <> contents <> "}"
  Pretty style -> do
    i <- asks indentLevels
    let objPair k v = newline <> indentBy (i + 1) <> objKey k <> ": " <> v
    contents <- fmap (T.intercalate "," . fmap (uncurry objPair)) . increment $ xs
    pure $ "{" <> contents <> newline <> indentBy i <> "}"
    where newline = "\n"
          indentBy = indenter style

compileDataset :: Formatting -> Dataset (Translation (Message Node)) -> Text
compileDataset fo ds = runReader (dataset ds) (Config fo 0)
  where dataset = obj . traverse (uncurry f) . M.toList
        f x = fmap (x,) . translation

translation :: Translation (Message Node) -> Compiler Text
translation Translation { message, backend, mdesc } = obj . pure . fromList $ ys
  where ys =
          [ ("message", strVal . ICU.compileMsg ICU.SingleLine $ message)
          , ("backend", backendVal)
          , ("description", maybe nullVal strVal mdesc)
          ]
        backendVal = strVal $
          case backend of
             TypeScript      -> "ts"
             TypeScriptReact -> "tsx"
