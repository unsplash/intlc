module Intlc.Compiler where

import qualified Data.Map                               as M
import qualified Intlc.Compiler.Backend.TypeScript      as TS
import qualified Intlc.Compiler.Backend.TypeScriptReact as TSX
import           Intlc.Core
import           Prelude

-- We'll `foldr` with `mempty`, avoiding `mconcat`, to preserve insertion order.
-- The `""` base case in `f` prevents a spare newline, acting like
-- intercalation.
dataset :: Dataset Translation -> Either (NonEmpty Text) Text
dataset = M.foldrWithKey ((merge .) . translation) (Right mempty)
  where
        -- Merge two `Right`s, essentially intercalating with newlines (hence
        -- the empty accumulator base case).
        merge (Right x) (Right "")  = Right x
        merge (Right x) (Right acc) = Right $ acc <> "\n" <> x
        -- Merge two `Left`s, or if one side is `Left` take that side,
        -- discarding any `Right`.
        merge (Left es) (Left acc)  = Left $ es <> acc
        merge es@(Left _) (Right _) = es
        merge (Right _) es@(Left _) = es

translation :: Text -> Translation -> Either (NonEmpty Text) Text
translation k (Translation v TypeScript)      = TS.export k v
translation k (Translation v TypeScriptReact) = TSX.export k v
