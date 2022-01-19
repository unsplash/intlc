-- This module mostly only concerns itself with what the type-level output will
-- look like. The value-level output is JavaScript and resides almost entirely
-- in the corresponding module. They have been written with one-another in mind.

module Intlc.Compiler.Backend.TypeScript (compileNamedExport) where

import           Data.List                         (nubBy)
import qualified Data.Text                         as T
import           Intlc.Compiler.Backend.JavaScript (InterpStrat (..),
                                                    Pieces (..),
                                                    compileStmtPieces)
import qualified Intlc.ICU                         as ICU
import           Prelude                           hiding (Type)
import           Utils                             ((<>^))

compileNamedExport :: InterpStrat -> Text -> ICU.Message -> Text
compileNamedExport x k v =
  let (p, n, r) = compileStmtPieces x k v
   in case p of
    ConstantPieces -> "export const " <> n <> ": " <> compileTypeof x v <> " = '" <> r <> "'"
    LambdaPieces   -> "export const " <> n <> ": " <> compileTypeof x v <> " = x => " <> r

compileTypeof :: InterpStrat -> ICU.Message -> Text
compileTypeof x = let o = fromStrat x in flip runReader o . typeof . fromMsg o

fromStrat :: InterpStrat -> Out
fromStrat TemplateLit = TUniOut TStr
fromStrat JSX         = TFragment

-- | A representation of the type-level output we will be compiling. It's a
-- little verbose split into these various sum types, but in doing so it's
-- correct by construction.
data TypeOf
  = Constant
  | Lambda Args Out

-- This should really be `NonEmpty`, but we can't guarantee that based upon the
-- ICU AST.
--
-- Avoid `Map` due to its `Ord` constraint.
type Args = [(Text, In)]

data Uni
  = TStr

data In
  = TUniIn Uni
  | TStrLitUnion [Text]
  | TNumLitUnion [Text]
  | TNum
  | TDate
  -- An endomorphism on `Out`. Omitted as an argument to enforce that it's the
  -- same type as the output of the top-level `Lambda`.
  | TEndo

data Out
  = TUniOut Uni
  | TFragment

fromMsg :: Out -> ICU.Message -> TypeOf
fromMsg _ ICU.Static {}    = Constant
fromMsg x (ICU.Dynamic ys) = Lambda (fromToken =<< toList ys) x

fromToken :: ICU.Token -> Args
fromToken ICU.Plaintext {}      = mempty
fromToken (ICU.Interpolation x) = fromArg x

fromArg :: ICU.Arg -> Args
fromArg (ICU.Arg n ICU.String)                             = pure (n, TUniIn TStr)
fromArg (ICU.Arg n ICU.Number)                             = pure (n, TNum)
fromArg (ICU.Arg n ICU.Date {})                            = pure (n, TDate)
fromArg (ICU.Arg n (ICU.Plural (ICU.LitPlural ls mw)))     = (n, t) : (fromExactPluralCase =<< toList ls) <> foldMap fromPluralWildcard mw
  -- When there's no wildcard case we can compile to a union of number literals.
  where t = case mw of
              Just _  -> TNum
              Nothing -> TNumLitUnion . toList $ caseLit <$> ls
        caseLit (ICU.PluralCase (ICU.PluralExact x) _)     = x
fromArg (ICU.Arg n (ICU.Plural (ICU.RulePlural rs w)))     = (n, TNum) : (fromRulePluralCase =<< toList rs) <> fromPluralWildcard w
fromArg (ICU.Arg n (ICU.Plural (ICU.MixedPlural ls rs w))) = (n, TNum) : (fromExactPluralCase =<< toList ls) <> (fromRulePluralCase =<< toList rs) <> fromPluralWildcard w
fromArg (ICU.Arg n (ICU.Select cs mw))                     = (n, t) : (fromSelectCase =<< toList cs) <> foldMap fromSelectWildcard mw
  -- When there's no wildcard case we can compile to a union of string literals.
  where t = case mw of
              Just _  -> TUniIn TStr
              Nothing -> TStrLitUnion . toList $ caseLit <$> cs
        caseLit (ICU.SelectCase x _) = x
fromArg (ICU.Arg n (ICU.Callback xs))                      = (n, TEndo) : (fromToken =<< xs)

fromExactPluralCase :: ICU.PluralCase ICU.PluralExact -> Args
fromExactPluralCase (ICU.PluralCase (ICU.PluralExact _) xs) = fromToken =<< xs

fromRulePluralCase :: ICU.PluralCase ICU.PluralRule -> Args
fromRulePluralCase (ICU.PluralCase _ xs) = fromToken =<< xs

fromPluralWildcard :: ICU.PluralWildcard -> Args
fromPluralWildcard (ICU.PluralWildcard xs) = fromToken =<< xs

fromSelectCase :: ICU.SelectCase -> Args
fromSelectCase (ICU.SelectCase _ xs) = fromToken =<< xs

fromSelectWildcard :: ICU.SelectWildcard -> Args
fromSelectWildcard (ICU.SelectWildcard xs) = fromToken =<< xs

type Compiler = Reader Out

-- The parameter name is functionally irrelevant in TypeScript type signatures.
argName :: Text
argName = "x"

union :: [Text] -> Text
union = T.intercalate " | "

typeof :: TypeOf -> Compiler Text
typeof Constant      = uni TStr
typeof (Lambda as r) = lambda as r

lambda :: Args -> Out -> Compiler Text
lambda as r = args (dedupe as) <>^ pure " => " <>^ out r
  where dedupe = nubBy ((==) `on` fst)

args :: Args -> Compiler Text
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
