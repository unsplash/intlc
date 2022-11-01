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
  x@(ICU.BoolF n _ _ _) -> (n, TBool) : fold x
  x@(ICU.StringF n _)   -> (n, TStr)  : fold x
  x@(ICU.NumberF n _)   -> (n, TNum)  : fold x
  x@(ICU.DateF n _ _)   -> (n, TDate) : fold x
  x@(ICU.TimeF n _ _)   -> (n, TDate) : fold x
  -- We can compile exact cardinal plurals (i.e. those without a wildcard) to a
  -- union of number literals.
  x@(ICU.CardinalExactF n ls _)      -> (n, t) : fold x where
    t = TNumLitUnion $ caseLit <$> ls
    caseLit (ICU.PluralExact y, _) = y
  x@(ICU.CardinalInexactF n _ _ _ _) -> (n, TNum) : fold x
  x@(ICU.OrdinalF n _ _ _ _)         -> (n, TNum) : fold x
  x@(ICU.SelectWildF n _ _)          -> (n, TStr) : fold x
  x@(ICU.SelectNamedWildF n _ _ _)   -> (n, TStr) : fold x
  -- When there's no wildcard case we can compile to a union of string literals.
  x@(ICU.SelectNamedF n cs _)        -> (n, TStrLitUnion (fst <$> cs)) : fold x
  x@(ICU.CallbackF n _ _)            -> (n, TEndo) : fold x
  -- Plural references are treated as a no-op.
  x -> fold x
