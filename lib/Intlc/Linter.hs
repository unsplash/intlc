module Intlc.Linter where

import qualified Data.Text             as T

import           Control.Monad.Extra   (pureIf)
import           Data.Char             (isAscii)
import           Data.Functor.Foldable (cata)
import qualified Data.Map              as M
import           Intlc.Core
import           Intlc.ICU
import           Prelude

data ExternalLint
  = RedundantSelect (NonEmpty Arg)
  | RedundantPlural (NonEmpty Arg)
  deriving (Eq, Show)

data InternalLint
  = TooManyInterpolations (NonEmpty Arg)
  | InvalidNonAsciiCharacter (NonEmpty Char)
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

type Rule a = Node -> Maybe a

lintWith :: [Rule a] -> Message -> Status a
lintWith rules (Message stream) = toStatus $ rules `flap` stream
  where toStatus = maybeToStatus . nonEmpty . catMaybes

lintExternal :: Message -> Status ExternalLint
lintExternal = lintWith
  [ redundantSelectRule
  , redundantPluralRule
  ]

lintInternal :: Message -> Status InternalLint
lintInternal = lintWith
  [ interpolationsRule
  , unsupportedUnicodeRule
  ]

-- Get the printable output from linting an entire dataset, if any.
lintDatasetWith :: (Message -> Status a) -> (Text -> NonEmpty a -> Text) -> Dataset Translation -> Maybe Text
lintDatasetWith linter fmt xs = pureIf (not $ M.null lints) msg
  where lints = M.mapMaybe (statusToMaybe . linter . message) xs
        msg = T.intercalate "\n" $ uncurry fmt <$> M.assocs lints

lintDatasetExternal :: Dataset Translation -> Maybe Text
lintDatasetExternal = lintDatasetWith lintExternal . formatFailureWith $ \case
  RedundantSelect xs -> "Redundant select: " <> T.intercalate ", " (fmap unArg . toList $ xs)
  RedundantPlural xs -> "Redundant plural: " <> T.intercalate ", " (fmap unArg . toList $ xs)

lintDatasetInternal :: Dataset Translation -> Maybe Text
lintDatasetInternal = lintDatasetWith lintInternal . formatFailureWith $ \case
  TooManyInterpolations xs         -> "Multiple complex interpolations: " <> T.intercalate ", " (fmap unArg . toList $ xs)
  (InvalidNonAsciiCharacter chars) -> "Following characters are not allowed: " <> intercalateChars chars
    where intercalateChars:: NonEmpty Char -> Text
          intercalateChars = T.intercalate " " . toList . fmap T.singleton

formatFailureWith :: (Functor f, Foldable f) => (a -> Text) -> Text -> f a -> Text
formatFailureWith f k es = title <> msgs
  where title = k <> ": \n"
        msgs = T.intercalate "\n" . toList . fmap (indent . f) $ es
        indent = (" " <>)

-- Select interpolations with only wildcards are redundant: they could be
-- replaced with plain string interpolations.
redundantSelectRule :: Rule ExternalLint
redundantSelectRule = fmap RedundantSelect . nonEmpty . idents where
  idents = cata $ \case
    SelectWildF n xs ys -> n : xs <> ys
    x                   -> fold x

-- Plural interpolations with only wildcards are redundant: they could be
-- replaced with plain number interpolations.
redundantPluralRule :: Rule ExternalLint
redundantPluralRule = fmap RedundantPlural . nonEmpty . idents where
  idents = cata $ \case
    CardinalInexactF n [] [] xs ys -> n : xs <> ys
    OrdinalF         n [] [] xs ys -> n : xs <> ys
    x                              -> fold x

-- Our translation vendor has poor support for ICU syntax, and their parser
-- particularly struggles with interpolations. This rule limits the use of a
-- subset of interpolations to one per message.
--
-- Callbacks and plurals are allowed an unlimited number of times. The former
-- because the vendor's tool has no issues parsing its syntax and the latter
-- because it's a special case that we can't rewrite.
interpolationsRule :: Rule InternalLint
interpolationsRule = count . idents where
  count (x:y:zs) = Just . TooManyInterpolations $ x :| (y:zs)
  count _        = Nothing
  idents = cata $ \case
    BoolF n xs ys zs            -> n : xs <> ys <> zs
    SelectNamedF n xs ys        -> n : selCases xs <> ys
    SelectWildF n xs ys         -> n : xs <> ys
    SelectNamedWildF n xs ys zs -> n : selCases xs <> ys <> zs
    x                           -> fold x
  selCases = foldMap snd

-- Allows any ASCII character as well as a handful of Unicode characters that
-- we've established are safe for use with our vendor's tool.
isAcceptedChar :: Char -> Bool
isAcceptedChar c = isAscii c || c `elem` acceptedChars
  where acceptedChars = ['’','…','é','—','ƒ','“','”','–']

unsupportedUnicodeRule :: Rule InternalLint
unsupportedUnicodeRule = output . nonAscii where
  output = fmap InvalidNonAsciiCharacter . nonEmpty
  nonAscii = cata $ \case
    CharF c xs -> guarded (not . isAcceptedChar) c <> xs
    x          -> fold x
