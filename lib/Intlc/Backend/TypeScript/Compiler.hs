-- This module mostly only concerns itself with what the type-level output will
-- look like. The value-level output is JavaScript and resides almost entirely
-- in the corresponding module. They have been written with one-another in mind.

module Intlc.Backend.TypeScript.Compiler (compileNamedExport, compileTypeof, validateKey) where

import qualified Data.Map                          as M
import qualified Data.Text                         as T
import           Intlc.Backend.JavaScript.Compiler (InterpStrat (..))
import qualified Intlc.Backend.JavaScript.Compiler as JS
import           Intlc.Backend.TypeScript.Language
import           Intlc.Core
import qualified Intlc.ICU                         as ICU
import           Prelude
import           Utils                             ((<>^))

compileNamedExport :: InterpStrat -> Locale -> Text -> ICU.Message -> Text
compileNamedExport s l k v = JS.compileStmt o s l k v
  where o = JS.emptyOverrides { JS.stmtOverride = Just stmt, JS.matchLitCondOverride = Just matchLitCond }
        stmt n r = "export const " <> n <> ": " <> compileTypeof s v <> " = " <> arg <> " => " <> r
        -- Prevents TypeScript from narrowing, which absent this causes some
        -- nested switch output to fail typechecking.
        matchLitCond x = x <> " as typeof " <> x
        arg = if hasInterpolations (ICU.unMessage v) then "x" else "()"
        hasInterpolations ICU.Fin        = False
        hasInterpolations (ICU.Char _ n) = hasInterpolations n
        hasInterpolations _              = True

compileTypeof :: InterpStrat -> ICU.Message -> Text
compileTypeof x = let o = fromStrat x in flip runReader o . typeof . fromMsg o

fromStrat :: InterpStrat -> Out
fromStrat TemplateLit = TTemplate
fromStrat JSX         = TFragment

type Compiler = Reader Out

-- The parameter name is functionally irrelevant in TypeScript type signatures.
argName :: Text
argName = "x"

union :: Foldable f => f Text -> Text
union = T.intercalate " | " . toList

typeof :: TypeOf -> Compiler Text
typeof (Lambda as r) = lambda as r

lambda :: Args -> Out -> Compiler Text
lambda as r = args as <>^ pure " => " <>^ out r

args :: Args -> Compiler Text
args xs
  | M.null xs = pure "()"
  | otherwise = do
    y <- fmap (T.intercalate "; " . M.elems) . M.traverseWithKey arg $ xs
    pure $ "(" <> argName <> ": { " <> y <> " })"
      where arg (ICU.Arg k) (v :| []) = ((k <> ": ") <>) <$> in' v
            arg (ICU.Arg k) vs        = ((k <> ": ") <>) . intersect . toList <$> ins `mapM` vs
            -- Unions with at least two members need wrapping in disambiguating
            -- parentheses, other types do not.
            ins x
              | isMultiUnion x = parens <$> in' x
              | otherwise      = in' x
            intersect = T.intercalate " & "
            parens x = "(" <> x <> ")"

in' :: In -> Compiler Text
in' TStr              = pure "string"
in' (TStrLitUnion xs) = pure . union $ qts <$> xs
  where qts x = "'" <> x <> "'"
in' (TNumLitUnion xs) = pure . union $ xs
in' TNum              = pure "number"
in' TBool             = pure "boolean"
in' TDate             = pure "Date"
in' TEndo             = endo

out :: Out -> Compiler Text
out TTemplate = pure "string"
out TFragment = pure "ReactElement"

endo :: Compiler Text
endo = do
  x <- out =<< ask
  pure $ "(" <> argName <> ": " <> x <> ") => " <> x

-- Words like `namespace` and `type` aren't reserved at the value-level. `enum`
-- is already reserved in JS spec. As such, we can directly reuse JS key
-- validation.
validateKey :: Text -> Either Text ()
validateKey = JS.validateKey
