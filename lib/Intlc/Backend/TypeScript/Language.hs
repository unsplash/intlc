module Intlc.Backend.TypeScript.Language where

import           Data.List.NonEmpty (nub)
import qualified Data.Map           as M
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
fromMsg x (ICU.Message y) = Lambda (collateArgs . fromNode $ y) x

fromNode :: ICU.Node -> UncollatedArgs
fromNode ICU.Fin               = mempty
fromNode (ICU.Char _ x)        = fromNode x
fromNode (ICU.Bool n x y z)    = (n, TBool) : foldMap fromNode [x, y, z]
fromNode (ICU.String n x)      = pure (n, TStr) <> fromNode x
fromNode (ICU.Number n x)      = pure (n, TNum) <> fromNode x
fromNode (ICU.Date n _ x)      = pure (n, TDate) <> fromNode x
fromNode (ICU.Time n _ x)      = pure (n, TDate) <> fromNode x
-- We can compile exact cardinal plurals (i.e. those without a wildcard) to a
-- union of number literals.
fromNode (ICU.CardinalExact n ls x)        = (n, t) : (fromExactPluralCase =<< toList ls) <> fromNode x
  where t = TNumLitUnion $ caseLit <$> ls
        caseLit (ICU.PluralExact y, _) = y
fromNode (ICU.CardinalInexact n ls rs w x) = (n, TNum) : (fromExactPluralCase =<< ls) <> (fromRulePluralCase =<< rs) <> foldMap fromNode [w, x]
fromNode (ICU.Ordinal n ls rs w x)         = (n, TNum) : (fromExactPluralCase =<< ls) <> (fromRulePluralCase =<< rs) <> foldMap fromNode [w, x]
-- Plural references are treated as a no-op.
fromNode (ICU.PluralRef _ x)               = fromNode x
fromNode (ICU.SelectWild n w x)            = (n, TStr) : foldMap fromNode [w, x]
fromNode (ICU.SelectNamedWild n cs w x)    = (n, TStr) : (fromSelectCase =<< toList cs) <> foldMap fromNode [w, x]
-- When there's no wildcard case we can compile to a union of string literals.
fromNode (ICU.SelectNamed n cs x)          = (n, TStrLitUnion (fst <$> cs)) : (fromSelectCase =<< toList cs) <> fromNode x
fromNode (ICU.Callback n x y)              = (n, TEndo) : foldMap fromNode [x, y]

fromExactPluralCase :: ICU.PluralCase ICU.PluralExact -> UncollatedArgs
fromExactPluralCase = fromNode . snd

fromRulePluralCase :: ICU.PluralCase ICU.PluralRule -> UncollatedArgs
fromRulePluralCase = fromNode . snd

fromSelectCase :: ICU.SelectCase -> UncollatedArgs
fromSelectCase = fromNode . snd
