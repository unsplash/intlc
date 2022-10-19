module Intlc.Backend.TypeScript.Language where

import           Data.Functor.Foldable (cata)
import           Data.List.NonEmpty    (nub)
import qualified Data.Map              as M
import qualified Intlc.ICU             as ICU
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
fromNode = cata $ \case
  (ICU.BoolF n xs ys zs) -> (n, TBool) : xs <> ys <> zs
  (ICU.StringF n xs)     -> pure (n, TStr) <> xs
  (ICU.NumberF n xs)     -> pure (n, TNum) <> xs
  (ICU.DateF n _ xs)     -> pure (n, TDate) <> xs
  (ICU.TimeF n _ xs)     -> pure (n, TDate) <> xs
  -- We can compile exact cardinal plurals (i.e. those without a wildcard) to a
  -- union of number literals.
  (ICU.CardinalExactF n ls xs)         ->
    let t = TNumLitUnion $ caseLit <$> ls
        caseLit (ICU.PluralExact y, _) = y
     in (n, t) : (fromPluralCase =<< toList ls) <> xs
  (ICU.CardinalInexactF n ls rs ws xs) -> (n, TNum) : (fromPluralCase =<< ls) <> (fromPluralCase =<< rs) <> ws <> xs
  (ICU.OrdinalF n ls rs ws xs)         -> (n, TNum) : (fromPluralCase =<< ls) <> (fromPluralCase =<< rs) <> ws <> xs
  (ICU.SelectWildF n ws xs)            -> (n, TStr) : ws <> xs
  (ICU.SelectNamedWildF n cs ws xs)    -> (n, TStr) : (fromSelectCase =<< toList cs) <> ws <> xs
  -- When there's no wildcard case we can compile to a union of string literals.
  (ICU.SelectNamedF n cs xs)           -> (n, TStrLitUnion (fst <$> cs)) : (fromSelectCase =<< toList cs) <> xs
  (ICU.CallbackF n xs ys)              -> (n, TEndo) : xs <> ys
  -- Plural references are treated as a no-op.
  x -> fold x

fromPluralCase :: ICU.PluralCaseF a b -> b
fromPluralCase = snd

fromSelectCase :: ICU.SelectCaseF a -> a
fromSelectCase = snd
