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
fromMsg x (ICU.Message ys) = Lambda (collateArgs (fromNode =<< toList ys)) x

fromNode :: ICU.Node -> UncollatedArgs
fromNode ICU.Plaintext {}    = mempty
fromNode (ICU.Bool n xs ys)  = (n, TBool) : (fromNode =<< xs) <> (fromNode =<< ys)
fromNode (ICU.String n)      = pure (n, TStr)
fromNode (ICU.Number n)      = pure (n, TNum)
fromNode (ICU.Date n _)      = pure (n, TDate)
fromNode (ICU.Time n _)      = pure (n, TDate)
-- We can compile exact cardinal plurals (i.e. those without a wildcard) to a
-- union of number literals.
fromNode (ICU.CardinalExact n ls)        = (n, t) : (fromExactPluralCase =<< toList ls)
  where t = TNumLitUnion $ caseLit <$> ls
        caseLit (ICU.PluralExact x, _) = x
fromNode (ICU.CardinalInexact n ls rs w) = (n, TNum) : (fromExactPluralCase =<< ls) <> (fromRulePluralCase =<< rs) <> (fromNode =<< w)
fromNode (ICU.Ordinal n ls rs w)         = (n, TNum) : (fromExactPluralCase =<< ls) <> (fromRulePluralCase =<< rs) <> (fromNode =<< w)
-- Plural references are treated as a no-op.
fromNode ICU.PluralRef {}    = mempty
fromNode (ICU.Select n x)    = case x of
  (That w)     -> (n, TStr) : (fromNode =<< w)
  (These cs w) -> (n, TStr) : (fromSelectCase =<< toList cs) <> (fromNode =<< w)
  -- When there's no wildcard case we can compile to a union of string literals.
  (This cs)    -> (n, TStrLitUnion (fst <$> cs)) : (fromSelectCase =<< toList cs)
fromNode (ICU.Callback n xs) = (n, TEndo) : (fromNode =<< xs)

fromExactPluralCase :: ICU.PluralCase ICU.PluralExact -> UncollatedArgs
fromExactPluralCase (ICU.PluralExact _, xs) = fromNode =<< xs

fromRulePluralCase :: ICU.PluralCase ICU.PluralRule -> UncollatedArgs
fromRulePluralCase (_, xs) = fromNode =<< xs

fromSelectCase :: ICU.SelectCase -> UncollatedArgs
fromSelectCase (_, xs) = fromNode =<< xs
