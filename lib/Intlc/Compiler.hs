module Intlc.Compiler where

import qualified Data.Map                          as M
import           Intlc.Compiler.Backend.JavaScript (InterpStrat (..))
import qualified Intlc.Compiler.Backend.TypeScript as TS
import           Intlc.Compiler.Common             (validateArgs)
import           Intlc.Core
import qualified Intlc.ICU                         as ICU
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
translation k (Translation v be) = validateArgs (args v) $> case be of
  TypeScript      -> TS.compileNamedExport TemplateLit k v
  TypeScriptReact -> TS.compileNamedExport JSX         k v

args :: ICU.Message -> [ICU.Arg]
args ICU.Static {}    = []
args (ICU.Dynamic xs) = token `mapMaybe` xs
  where token ICU.Plaintext {}      = Nothing
        token (ICU.Interpolation x) = Just x
