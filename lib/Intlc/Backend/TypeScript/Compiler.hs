-- This module mostly only concerns itself with what the type-level output will
-- look like. The value-level output is JavaScript and resides almost entirely
-- in the corresponding module. They have been written with one-another in mind.

module Intlc.Backend.TypeScript.Compiler (compileNamedExport, compileTypeof) where

import           Data.List                         (nubBy)
import qualified Data.Text                         as T
import           Intlc.Backend.JavaScript.Compiler (InterpStrat (..),
                                                    compileStmtPieces)
import           Intlc.Backend.TypeScript.Language
import           Intlc.Core
import qualified Intlc.ICU                         as ICU
import           Prelude                           hiding (Type)
import           Utils                             ((<>^))

compileNamedExport :: InterpStrat -> Locale -> Text -> ICU.Message -> Text
compileNamedExport x l k v =
  let (n, r) = compileStmtPieces x l k v
      arg = case v of
        ICU.Static {}  -> "()"
        ICU.Dynamic {} -> "x"
   in "export const " <> n <> ": " <> compileTypeof x v <> " = " <> arg <> " => " <> r

compileTypeof :: InterpStrat -> ICU.Message -> Text
compileTypeof x = let o = fromStrat x in flip runReader o . typeof . fromMsg o

fromStrat :: InterpStrat -> Out
fromStrat TemplateLit = TUniOut TStr
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
lambda as r = args (dedupe as) <>^ pure " => " <>^ out r
  where dedupe = nubBy ((==) `on` fst)

args :: Args -> Compiler Text
args [] = pure "()"
args xs = do
  y <- T.intercalate "; " <$> mapM (uncurry arg) xs
  pure $ "(" <> argName <> ": { " <> y <> " })"
    where arg k v = ((k <> ": ") <>) <$> in' v

uni :: Uni -> Compiler Text
uni TStr = pure "string"

in' :: In -> Compiler Text
in' (TUniIn x)        = uni x
in' (TStrLitUnion xs) = pure . union $ qts <$> xs
  where qts x = "'" <> x <> "'"
in' (TNumLitUnion xs) = pure . union $ xs
in' TNum              = pure "number"
in' TDate             = pure "Date"
in' TEndo             = endo

out :: Out -> Compiler Text
out (TUniOut x) = uni x
out TFragment   = pure "ReactElement"

endo :: Compiler Text
endo = do
  x <- out =<< ask
  pure $ "(" <> argName <> ": " <> x <> ") => " <> x
