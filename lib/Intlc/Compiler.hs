module Intlc.Compiler where

import qualified Data.Map                               as M
import qualified Intlc.Compiler.Backend.TypeScript      as TS
import qualified Intlc.Compiler.Backend.TypeScriptReact as TSX
import           Intlc.Core
import           Prelude

dataset :: Dataset Translation -> Text
dataset = M.foldMapWithKey translation

translation :: Text -> Translation -> Text
translation k (Translation v TypeScript)      = TS.export k v
translation k (Translation v TypeScriptReact) = TSX.export k v
