module Intlc.Backend.TypeScript.Language where

import           Data.List.NonEmpty (nub)
import qualified Data.Map           as M
import           Data.These         (These (..))
import qualified Intlc.ICU          as ICU
import           Prelude

-- | A representation of the type-level output we will be compiling. It's a
-- little verbose split into these various sum types, but in doing so it's
-- correct by construction.
data TypeOf = Lambda Args Out
  deriving (Show, Eq)

type UncollatedArgs = [(ICU.Arg, In)]
type Args = Map ICU.Arg (NonEmpty In)

data In
  = TStr
  | TStrLitUnion (NonEmpty Text)
  | TNumLitUnion (NonEmpty Text)
  | TNum
  | TBool
  | TDate
  -- An endomorphism on `Out`. Omitted as an argument to enforce that it's the
  -- same type as the output of the top-level `Lambda`.
  | TEndo
  deriving (Show, Eq)

data Out
  = TTemplate
  | TFragment
  deriving (Show, Eq)

isMultiUnion :: In -> Bool
isMultiUnion (TStrLitUnion xs) = length xs > 1
isMultiUnion (TNumLitUnion xs) = length xs > 1
isMultiUnion _                 = False

-- Collate arguments with the same name.
collateArgs :: UncollatedArgs -> Args
collateArgs = fmap nub . M.fromListWith (<>) . fmap (second pure)

fromMsg :: Out -> ICU.Message -> TypeOf
fromMsg x (ICU.Message ys) = Lambda (collateArgs (fromToken =<< toList ys)) x

fromToken :: ICU.Token -> UncollatedArgs
fromToken ICU.Plaintext {}    = mempty
fromToken (ICU.Bool n xs ys)  = (n, TBool) : (fromToken =<< xs) <> (fromToken =<< ys)
fromToken (ICU.String n)      = pure (n, TStr)
fromToken (ICU.Number n)      = pure (n, TNum)
fromToken (ICU.Date n _)      = pure (n, TDate)
fromToken (ICU.Time n _)      = pure (n, TDate)
fromToken (ICU.Plural n x)    = fromPlural n x
-- Plural references are treated as a no-op.
fromToken ICU.PluralRef {}    = mempty
fromToken (ICU.Select n x)    = case x of
  (That w)     -> (n, TStr) : fromSelectWildcard w
  (These cs w) -> (n, TStr) : (fromSelectCase =<< toList cs) <> fromSelectWildcard w
  -- When there's no wildcard case we can compile to a union of string literals.
  (This cs)    -> (n, TStrLitUnion (lit <$> cs)) : (fromSelectCase =<< toList cs)
    where lit (ICU.SelectCase l _) = l
fromToken (ICU.Callback n xs) = (n, TEndo) : (fromToken =<< xs)

fromPlural :: ICU.Arg -> ICU.Plural -> UncollatedArgs
-- We can compile exact cardinal plurals (i.e. those without a wildcard) to a
-- union of number literals.
fromPlural n (ICU.CardinalExact ls)        = (n, t) : (fromExactPluralCase =<< toList ls)
  where t = TNumLitUnion $ caseLit <$> ls
        caseLit (ICU.PluralCase (ICU.PluralExact x) _) = x
fromPlural n (ICU.CardinalInexact ls rs w) = (n, TNum) : (fromExactPluralCase =<< ls) <> (fromRulePluralCase =<< rs) <> fromPluralWildcard w
fromPlural n (ICU.Ordinal ls rs w)         = (n, TNum) : (fromExactPluralCase =<< ls) <> (fromRulePluralCase =<< rs) <> fromPluralWildcard w

fromExactPluralCase :: ICU.PluralCase ICU.PluralExact -> UncollatedArgs
fromExactPluralCase (ICU.PluralCase (ICU.PluralExact _) xs) = fromToken =<< xs

fromRulePluralCase :: ICU.PluralCase ICU.PluralRule -> UncollatedArgs
fromRulePluralCase (ICU.PluralCase _ xs) = fromToken =<< xs

fromPluralWildcard :: ICU.PluralWildcard -> UncollatedArgs
fromPluralWildcard (ICU.PluralWildcard xs) = fromToken =<< xs

fromSelectCase :: ICU.SelectCase -> UncollatedArgs
fromSelectCase (ICU.SelectCase _ xs) = fromToken =<< xs

fromSelectWildcard :: ICU.SelectWildcard -> UncollatedArgs
fromSelectWildcard (ICU.SelectWildcard xs) = fromToken =<< xs
