module Intlc.Backend.JSON.Compiler where

import           Data.List.Extra            (escapeJSON)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Intlc.Backend.ICU.Compiler (Formatting (SingleLine),
                                             compileMsg)
import           Intlc.Core
import           Prelude

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

objPair :: Text -> Text -> Text
objPair k v = objKey k <> ":" <> v

obj :: [(Text, Text)] -> Text
obj xs = "{" <> ys <> "}"
  where ys = T.intercalate "," . fmap (uncurry objPair) $ xs

compileDataset :: Dataset Translation -> Text
compileDataset = obj . M.toList . M.map translation

translation :: Translation -> Text
translation Translation { message, backend, mdesc } = obj . fromList $ ys
  where ys =
          [ ("message", strVal . compileMsg SingleLine $ message)
          , ("backend", backendVal)
          , ("description", maybe nullVal strVal mdesc)
          ]
        backendVal = strVal $
          case backend of
             TypeScript      -> "ts"
             TypeScriptReact -> "tsx"
