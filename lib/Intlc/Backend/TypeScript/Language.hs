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
fromNode = cata $ (maybeToList . marg) <> fold

marg :: ICU.NodeF a -> Maybe (ICU.Arg, In)
marg = \case
  ICU.BoolF n _ _ _              -> pure (n, TBool)
  ICU.StringF n _                -> pure (n, TStr)
  ICU.NumberF n _                -> pure (n, TNum)
  ICU.DateF n _ _                -> pure (n, TDate)
  ICU.TimeF n _ _                -> pure (n, TDate)
  -- We can compile exact cardinal plurals (i.e. those without a wildcard) to a
  -- union of number literals.
  ICU.CardinalExactF n ls _      -> pure (n, TNumLitUnion (caseLit <$> ls))
    where caseLit (ICU.PluralExact y, _) = y
  ICU.CardinalInexactF n _ _ _ _ -> pure (n, TNum)
  ICU.OrdinalF n _ _ _ _         -> pure (n, TNum)
  ICU.SelectWildF n _ _          -> pure (n, TStr)
  ICU.SelectNamedWildF n _ _ _   -> pure (n, TStr)
  -- When there's no wildcard case we can compile to a union of string literals.
  ICU.SelectNamedF n cs _        -> pure (n, TStrLitUnion (fst <$> cs))
  ICU.CallbackF n _ _            -> pure (n, TEndo)
  -- Plural references are treated as a no-op.
  _                              -> empty
