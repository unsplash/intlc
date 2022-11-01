module Intlc.Linter where

import qualified Data.Text                     as T

import           Control.Comonad               (extract)
import           Control.Comonad.Trans.Cofree  (CofreeF ((:<)), tailF)
import           Data.Char                     (isAscii)
import           Data.Functor.Foldable         (cata, para)
import           Intlc.Backend.ICU.Compiler    (pluralExact, pluralRule)
import           Intlc.Core
import           Intlc.ICU
import           Prelude
import           Text.Megaparsec               (PosState (PosState),
                                                defaultTabWidth, initialPos)
import           Text.Megaparsec.Error
import           Text.Megaparsec.Error.Builder
import           Utils                         (bunBy)

type WithAnn a = (Int, a)

type AnnExternalLint = WithAnn ExternalLint
type AnnInternalLint = WithAnn InternalLint

data ExternalLint
  = RedundantSelect Arg
  | RedundantPlural Arg
  | DuplicateSelectCase Arg Text
  | DuplicatePluralCase Arg Text
  deriving (Eq, Show, Ord)

data InternalLint
  = TooManyInterpolations (NonEmpty Arg)
  | InvalidNonAsciiCharacter Char
  deriving (Eq,Show, Ord)

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

lintWith :: [Rule a] -> Message AnnNode -> Status a
lintWith rules (Message ast) = maybeToStatus . catNEMaybes . flap rules $ ast
  where catNEMaybes :: [Maybe (NonEmpty a)] -> Maybe (NonEmpty a)
        catNEMaybes = nonEmpty . foldMap (foldMap toList)

lintExternal :: Message AnnNode -> Status AnnExternalLint
lintExternal = lintWith
  [ redundantSelectRule
  , redundantPluralRule
  , duplicateSelectCasesRule
  , duplicatePluralCasesRule
  ]

lintInternal :: Message AnnNode -> Status AnnInternalLint
lintInternal = lintWith
  [ interpolationsRule
  , unsupportedUnicodeRule
  ]

-- We're going to reuse Megaparsec's infrastructure to format our lints.
fmt :: ShowErrorComponent a => FilePath -> Text -> NonEmpty (WithAnn a) -> Text
fmt path content lints = T.pack $ errorBundlePretty (buildParseErrBundle path content lints)

buildParseErrBundle :: FilePath -> Text -> NonEmpty (WithAnn a) -> ParseErrorBundle Text a
buildParseErrBundle path content lints = ParseErrorBundle (buildParseErr <$> lints) (buildPosState path content)

buildParseErr :: WithAnn a -> ParseError Text a
buildParseErr (i, x) = errFancy i . fancy . ErrorCustom $ x

-- This could probably be rewritten to be more efficient.
buildPosState :: FilePath -> Text -> PosState Text
buildPosState path content = PosState content 0 (initialPos path) defaultTabWidth mempty

-- Get the printable output from linting an entire dataset, if any.
lintDatasetWith :: ShowErrorComponent a =>
  (Message AnnNode -> Status (WithAnn a)) -> FilePath -> Text -> Dataset (Translation (Message AnnNode)) -> Maybe Text
lintDatasetWith linter path content = fmap (fmt path content) . foldMap (statusToMaybe . linter . message)

lintDatasetExternal :: FilePath -> Text -> Dataset (Translation (Message AnnNode)) -> Maybe Text
lintDatasetExternal = lintDatasetWith lintExternal

lintDatasetInternal :: FilePath -> Text -> Dataset (Translation (Message AnnNode)) -> Maybe Text
lintDatasetInternal = lintDatasetWith lintInternal

wikify :: Text -> Text -> Text
wikify name content = name <> ": " <> content <> "\n\nLearn more: " <> link
  where link = "https://github.com/unsplash/intlc/wiki/Lint-rules-reference#" <> name

instance ShowErrorComponent ExternalLint where
  showErrorComponent = T.unpack . uncurry wikify . (wikiName &&& msg) where
    msg = \case
      RedundantSelect x       -> "Select named `" <> unArg x <> "` is redundant as it only contains a wildcard."
      RedundantPlural x       -> "Plural named `" <> unArg x <> "` is redundant as it only contains a wildcard."
      DuplicateSelectCase x y -> "Select named `" <> unArg x <> "` contains a duplicate case named `" <> y <> "`."
      DuplicatePluralCase x y -> "Plural named `" <> unArg x <> "` contains a duplicate `" <> y <> "` case."
    wikiName = \case
      RedundantSelect {}     -> "redundant-select"
      RedundantPlural {}     -> "redundant-plural"
      DuplicateSelectCase {} -> "duplicate-select-case"
      DuplicatePluralCase {} -> "duplicate-plural-case"
  errorComponentLen = \case
    RedundantSelect x       -> T.length (unArg x)
    RedundantPlural x       -> T.length (unArg x)
    DuplicateSelectCase _ x -> T.length x
    DuplicatePluralCase _ x -> T.length x

instance ShowErrorComponent InternalLint where
  showErrorComponent = T.unpack . uncurry wikify . (wikiName &&& msg) where
    msg = \case
      TooManyInterpolations xs   -> "Multiple \"complex\" non-plural interpolations in the same message are disallowed. Found names: " <> interps
        where interps = T.intercalate ", " (fmap (qts . unArg) . toList $ xs)
              qts x = "`" <> x <> "`"
      InvalidNonAsciiCharacter x -> "Non-ASCII character `" <> T.singleton x <> "` is disallowed."
    wikiName = \case
      TooManyInterpolations {}    -> "too-many-interpolations"
      InvalidNonAsciiCharacter {} -> "invalid-non-ascii-char"
  errorComponentLen = \case
    TooManyInterpolations {}    -> 1
    InvalidNonAsciiCharacter {} -> 1

-- Select interpolations with only wildcards are redundant: they could be
-- replaced with plain string interpolations.
redundantSelectRule :: Rule AnnExternalLint
redundantSelectRule = nonEmpty . idents where
  idents = cata $ (maybeToList . mident) <> fold
  mident = \case
    i :< SelectWildF n _ _ -> pure (i + 1, RedundantSelect n)
    _                      -> empty

-- Plural interpolations with only wildcards are redundant: they could be
-- replaced with plain number interpolations.
redundantPluralRule :: Rule AnnExternalLint
redundantPluralRule = nonEmpty . idents where
  idents = cata $ (maybeToList . mident) <> fold
  mident = \case
    i :< CardinalInexactF n [] [] _ _ -> pure $ f i n
    i :< OrdinalF         n [] [] _ _ -> pure $ f i n
    _                                 -> empty
  f i n = (i + 1, RedundantPlural n)

-- Duplicate case names in select interpolations are redundant.
duplicateSelectCasesRule :: Rule AnnExternalLint
duplicateSelectCasesRule = nonEmpty . cases where
  cases = para $ (hereCases . tailF) <> foldMap snd
  hereCases = \case
    SelectNamedF n xs _       -> here n xs
    SelectNamedWildF n xs _ _ -> here n xs
    _                         -> mempty
  here n = fmap (uncurry (f n) . (caseOffset &&& caseName)) . bunBy ((==) `on` caseName)
    where caseName = fst
          caseOffset = uncurry calcCaseNameOffset . caseHead
          caseHead = second (extract . fst)
  f n i x = (i, DuplicateSelectCase n x)

-- Duplicate cases in plural interpolations are redundant.
duplicatePluralCasesRule :: Rule AnnExternalLint
duplicatePluralCasesRule = nonEmpty . cases where
  cases = para $ (hereCases . tailF) <> foldMap snd
  hereCases = \case
    CardinalExactF n ys _        -> here pluralExact n ys
    CardinalInexactF n ys zs _ _ -> here pluralExact n ys <> here pluralRule n zs
    OrdinalF n ys zs _ _         -> here pluralExact n ys <> here pluralRule n zs
    _                            -> mempty
  here via n = fmap (uncurry (f n) . (caseOffset &&& (via . caseKey))) . bunBy ((==) `on` caseKey)
    where caseKey = fst
          caseOffset = uncurry calcCaseNameOffset . first via . caseHead
          caseHead = second (extract . fst)
  f n i x = (i, DuplicatePluralCase n x)

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
  idents = cata $ (maybeToList . mident . tailF) <> fold
  mident = \case
    BoolF n _ _ _            -> pure n
    SelectNamedF n _ _       -> pure n
    SelectWildF n _ _        -> pure n
    SelectNamedWildF n _ _ _ -> pure n
    _                        -> empty
  start = extract ast

-- Allows any ASCII character as well as a handful of Unicode characters that
-- we've established are safe for use with our vendor's tool.
isAcceptedChar :: Char -> Bool
isAcceptedChar c = isAscii c || c `elem` acceptedChars
  where acceptedChars = ['’','…','é','—','ƒ','“','”','–']

unsupportedUnicodeRule :: Rule AnnInternalLint
unsupportedUnicodeRule = nonEmpty . nonAscii where
  nonAscii = cata $ (maybeToList . mchar) <> fold
  mchar = \case
    i :< CharF c _ -> (i,) . InvalidNonAsciiCharacter <$> guarded (not . isAcceptedChar) c
    _              -> empty

-- If we have access to the offset of the head node of an interpolation case, we
-- we can deduce the offset of the start of the case name.
calcCaseNameOffset :: Text -> Int -> Int
calcCaseNameOffset n i = i - 2 - T.length n
