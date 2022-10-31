module Intlc.Linter where

import qualified Data.Text                    as T

import           Control.Comonad              (extract)
import           Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import           Control.Monad.Extra          (pureIf)
import           Data.Char                    (isAscii)
import           Data.Functor.Foldable        (cata)
import qualified Data.Map                     as M
import           Intlc.Backend.ICU.Compiler   (pluralExact, pluralRule)
import           Intlc.Core
import           Intlc.ICU
import           Prelude
import           Utils                        (bun)

type WithAnn a = (Int, a)

type AnnExternalLint = WithAnn ExternalLint
type AnnInternalLint = WithAnn InternalLint

data ExternalLint
  = RedundantSelect Arg
  | RedundantPlural Arg
  | DuplicateSelectCase Arg Text
  | DuplicatePluralCase Arg Text
  deriving (Eq, Show)

data InternalLint
  = TooManyInterpolations (NonEmpty Arg)
  | InvalidNonAsciiCharacter Char
  deriving (Eq,Show)

data Status a
  = Success
  | Failure (NonEmpty a)
  deriving (Eq, Show)

statusToMaybe :: Status a -> Maybe (NonEmpty a)
statusToMaybe Success      = Nothing
statusToMaybe (Failure xs) = Just xs

maybeToStatus :: Maybe (NonEmpty a) -> Status a
maybeToStatus Nothing   = Success
maybeToStatus (Just xs) = Failure xs

type Rule a = AnnNode -> Maybe (NonEmpty a)

lintWith :: [Rule a] -> AnnMessage -> Status a
lintWith rules (AnnMessage ast) = maybeToStatus . catNEMaybes . flap rules $ ast
  where catNEMaybes :: [Maybe (NonEmpty a)] -> Maybe (NonEmpty a)
        catNEMaybes = nonEmpty . foldMap (foldMap toList)

lintExternal :: AnnMessage -> Status AnnExternalLint
lintExternal = lintWith
  [ redundantSelectRule
  , redundantPluralRule
  , duplicateSelectCasesRule
  , duplicatePluralCasesRule
  ]

lintInternal :: AnnMessage -> Status AnnInternalLint
lintInternal = lintWith
  [ interpolationsRule
  , unsupportedUnicodeRule
  ]

-- Get the printable output from linting an entire dataset, if any.
lintDatasetWith :: (AnnMessage -> Status a) -> (Text -> NonEmpty a -> Text) -> Dataset (Translation AnnMessage) -> Maybe Text
lintDatasetWith linter fmt xs = pureIf (not $ M.null lints) msg
  where lints = M.mapMaybe (statusToMaybe . linter . message) xs
        msg = T.intercalate "\n" $ uncurry fmt <$> M.assocs lints

lintDatasetExternal :: Dataset (Translation AnnMessage) -> Maybe Text
lintDatasetExternal = lintDatasetWith lintExternal . formatFailureWith $ \case
  RedundantSelect x       -> "Redundant select: " <> unArg x
  RedundantPlural x       -> "Redundant plural: " <> unArg x
  DuplicateSelectCase x y -> "Duplicate select case: " <> unArg x <> ", " <> y
  DuplicatePluralCase x y -> "Duplicate plural case: " <> unArg x <> ", " <> y

lintDatasetInternal :: Dataset (Translation AnnMessage) -> Maybe Text
lintDatasetInternal = lintDatasetWith lintInternal . formatFailureWith $ \case
  TooManyInterpolations xs   -> "Multiple complex interpolations: " <> T.intercalate ", " (fmap unArg . toList $ xs)
  InvalidNonAsciiCharacter x -> "Following character disallowed: " <> T.singleton x

formatFailureWith :: (Functor f, Foldable f) => (a -> Text) -> Text -> f (WithAnn a) -> Text
formatFailureWith f k es = title <> msgs
  where title = k <> ": \n"
        msgs = T.intercalate "\n" . toList . fmap (indent . f . snd) $ es
        indent = (" " <>)

-- Select interpolations with only wildcards are redundant: they could be
-- replaced with plain string interpolations.
redundantSelectRule :: Rule AnnExternalLint
redundantSelectRule = nonEmpty . idents where
  idents = cata $ \case
    x@(i :< SelectWildF n _ _) -> f i n : fold x
    x                          ->         fold x
  f i n = (i, RedundantSelect n)

-- Plural interpolations with only wildcards are redundant: they could be
-- replaced with plain number interpolations.
redundantPluralRule :: Rule AnnExternalLint
redundantPluralRule = nonEmpty . idents where
  idents = cata $ \case
    x@(i :< CardinalInexactF n [] [] _ _) -> f i n : fold x
    x@(i :< OrdinalF         n [] [] _ _) -> f i n : fold x
    x                                     ->         fold x
  f i n = (i, RedundantPlural n)

-- Duplicate case names in select interpolations are redundant.
duplicateSelectCasesRule :: Rule AnnExternalLint
duplicateSelectCasesRule = nonEmpty . cases where
  cases = cata $ \case
    x@(i :< SelectNamedF n ys _)       -> here i n ys <> fold x
    x@(i :< SelectNamedWildF n ys _ _) -> here i n ys <> fold x
    x                                  ->                fold x
  here i n = fmap (f i n) . bun . fmap fst
  f i n x = (i, DuplicateSelectCase n x)

-- Duplicate cases in plural interpolations are redundant.
duplicatePluralCasesRule :: Rule AnnExternalLint
duplicatePluralCasesRule = nonEmpty . cases where
  cases = cata $ \case
    x@(i :< CardinalExactF n ys _)        -> hereExact i n ys <>                     fold x
    x@(i :< CardinalInexactF n ys zs _ _) -> hereExact i n ys <> hereRules i n zs <> fold x
    x@(i :< OrdinalF n ys zs _ _)         -> hereExact i n ys <> hereRules i n zs <> fold x
    x                                     ->                                         fold x
  hereExact i n = fmap (f i n . pluralExact) . bun . fmap fst
  hereRules i n = fmap (f i n . pluralRule)  . bun . fmap fst
  f i n x = (i, DuplicatePluralCase n x)

-- Our translation vendor has poor support for ICU syntax, and their parser
-- particularly struggles with interpolations. This rule limits the use of a
-- subset of interpolations to one per message.
--
-- Callbacks and plurals are allowed an unlimited number of times. The former
-- because the vendor's tool has no issues parsing its syntax and the latter
-- because it's a special case that we can't rewrite.
interpolationsRule :: Rule AnnInternalLint
interpolationsRule ast = fmap (pure . (start,)) . count . idents $ ast where
  count (x:y:zs) = Just . TooManyInterpolations $ x :| (y:zs)
  count _        = Nothing
  idents = cata $ \case
    x@(_ :< BoolF n _ _ _)            -> n : fold x
    x@(_ :< SelectNamedF n _ _)       -> n : fold x
    x@(_ :< SelectWildF n _ _)        -> n : fold x
    x@(_ :< SelectNamedWildF n _ _ _) -> n : fold x
    x                                 ->     fold x
  start = extract ast

-- Allows any ASCII character as well as a handful of Unicode characters that
-- we've established are safe for use with our vendor's tool.
isAcceptedChar :: Char -> Bool
isAcceptedChar c = isAscii c || c `elem` acceptedChars
  where acceptedChars = ['’','…','é','—','ƒ','“','”','–']

unsupportedUnicodeRule :: Rule AnnInternalLint
unsupportedUnicodeRule = nonEmpty . nonAscii where
  nonAscii = cata $ \case
    x@(i :< CharF c _) -> (f i <$> guarded (not . isAcceptedChar) c) <> fold x
    x                  ->                                               fold x
  f i c = (i, InvalidNonAsciiCharacter c)
