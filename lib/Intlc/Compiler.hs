module Intlc.Compiler where

import qualified Data.Map                               as M
import qualified Intlc.Compiler.Backend.TypeScript      as TS
import qualified Intlc.Compiler.Backend.TypeScriptReact as TSX
import           Intlc.Core
import           Prelude

-- We'll `foldr` with `mempty`, avoiding `mconcat`, to preserve insertion order.
-- The `""` base case in `f` prevents a spare newline, acting like
-- intercalation.
dataset :: Dataset Translation -> Text
dataset = M.foldrWithKey f mempty
  where f k v ""  = translation k v
        f k v acc = acc <> newline <> translation k v

translation :: Text -> Translation -> Text
translation k (Translation v TypeScript)      = TS.export k v
translation k (Translation v TypeScriptReact) = TSX.export k v

newline :: Text
newline = "\n"
