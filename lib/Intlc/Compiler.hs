module Intlc.Compiler (compileDataset) where

import qualified Data.Map                          as M
import           Intlc.Compiler.Backend.JavaScript (InterpStrat (..))
import qualified Intlc.Compiler.Backend.TypeScript as TS
import           Intlc.Compiler.Common             (validateArgs)
import           Intlc.Core
import qualified Intlc.ICU                         as ICU
import           Prelude

-- We'll `foldr` with `mempty`, avoiding `mconcat`, to preserve insertion order.
-- The `""` base case in `merge` prevents a spare newline, acting like
-- intercalation.
compileDataset :: Locale -> Dataset Translation -> Either (NonEmpty Text) Text
compileDataset l = M.foldrWithKey ((merge .) . translation l) (Right mempty)
  where
        -- Merge two `Right`s, essentially intercalating with newlines (hence
        -- the empty accumulator base case).
        merge (Right x) (Right "")  = Right x
        merge (Right x) (Right acc) = Right $ x <> "\n" <> acc
        -- Merge two `Left`s, or if one side is `Left` take that side,
        -- discarding any `Right`.
        merge (Left es) (Left acc)  = Left $ es <> acc
        merge es@(Left _) (Right _) = es
        merge (Right _) es@(Left _) = es

translation :: Locale -> Text -> Translation -> Either (NonEmpty Text) Text
translation l k (Translation v be) = validateArgs (args v) $> case be of
  TypeScript      -> TS.compileNamedExport TemplateLit l k v
  TypeScriptReact -> TS.compileNamedExport JSX         l k v

args :: ICU.Message -> [ICU.Arg]
args ICU.Static {}    = []
args (ICU.Dynamic xs) = token `mapMaybe` toList xs
  where token ICU.Plaintext {}      = Nothing
        token (ICU.Interpolation x) = Just x
