module Intlc.Backend.TypeScript.Language where

import           Data.List          (findIndex)
import           Data.List.NonEmpty (nub)
import qualified Intlc.ICU          as ICU
import           Optics             (ix)
import           Optics.Operators
import           Prelude

-- | A representation of the type-level output we will be compiling. It's a
-- little verbose split into these various sum types, but in doing so it's
-- correct by construction.
data TypeOf = Lambda Args Out
  deriving (Show, Eq)

-- Avoid `Map` due to its `Ord` constraint.
type UncollatedArgs = [(Text, In)]
type Args = [(Text, NonEmpty In)]

data Uni
  = TStr
  deriving (Show, Eq)

data In
  = TUniIn Uni
  | TStrLitUnion (NonEmpty Text)
  | TNumLitUnion (NonEmpty Text)
  | TNum
  | TDate
  -- An endomorphism on `Out`. Omitted as an argument to enforce that it's the
  -- same type as the output of the top-level `Lambda`.
  | TEndo
  deriving (Show, Eq)

data Out
  = TUniOut Uni
  | TFragment
  deriving (Show, Eq)

isMultiUnion :: In -> Bool
isMultiUnion (TStrLitUnion xs) = length xs > 1
isMultiUnion (TNumLitUnion xs) = length xs > 1
isMultiUnion _                 = False

-- Collate arguments with the same name.
collateArgs :: UncollatedArgs -> Args
collateArgs = reverse . fmap (second nub) . go [] where
  go acc []         = acc
  go acc ((n,t):xs) =
    case findIndex ((== n) . fst) acc of
      Nothing -> go ((n, pure t):acc) xs
      Just i  -> go (acc & ix i %~ second (<> pure t)) xs

fromMsg :: Out -> ICU.Message -> TypeOf
fromMsg x ICU.Static {}    = Lambda mempty x
fromMsg x (ICU.Dynamic ys) = Lambda (collateArgs (fromToken =<< toList ys)) x

fromToken :: ICU.Token -> UncollatedArgs
fromToken ICU.Plaintext {}      = mempty
fromToken (ICU.Interpolation x) = fromArg x

fromArg :: ICU.Arg -> UncollatedArgs
fromArg (ICU.Arg n ICU.String)         = pure (n, TUniIn TStr)
fromArg (ICU.Arg n ICU.Number)         = pure (n, TNum)
fromArg (ICU.Arg n ICU.Date {})        = pure (n, TDate)
fromArg (ICU.Arg n ICU.Time {})        = pure (n, TDate)
fromArg (ICU.Arg n (ICU.Plural x))     = fromPlural n x
fromArg (ICU.Arg n (ICU.Select cs mw)) = (n, t) : (fromSelectCase =<< toList cs) <> foldMap fromSelectWildcard mw
  -- When there's no wildcard case we can compile to a union of string literals.
  where t = case mw of
              Just _  -> TUniIn TStr
              Nothing -> TStrLitUnion $ caseLit <$> cs
        caseLit (ICU.SelectCase x _) = x
fromArg (ICU.Arg n (ICU.Callback xs))  = (n, TEndo) : (fromToken =<< xs)

fromPlural :: Text -> ICU.Plural -> UncollatedArgs
fromPlural n (ICU.Cardinal (ICU.LitPlural ls mw))      = (n, t) : (fromExactPluralCase =<< toList ls) <> foldMap fromPluralWildcard mw
  -- When there's no wildcard case we can compile to a union of number literals.
  where t = case mw of
              Just _  -> TNum
              Nothing -> TNumLitUnion $ caseLit <$> ls
        caseLit (ICU.PluralCase (ICU.PluralExact x) _) = x
fromPlural n (ICU.Cardinal (ICU.RulePlural rs w))      = (n, TNum) : (fromRulePluralCase =<< toList rs) <> fromPluralWildcard w
fromPlural n (ICU.Cardinal (ICU.MixedPlural ls rs w))  = (n, TNum) : (fromExactPluralCase =<< toList ls) <> (fromRulePluralCase =<< toList rs) <> fromPluralWildcard w
fromPlural n (ICU.Ordinal (ICU.OrdinalPlural ls rs w)) = (n, TNum) : (fromExactPluralCase =<< ls) <> (fromRulePluralCase =<< toList rs) <> fromPluralWildcard w

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
