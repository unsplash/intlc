module Intlc.Linter where

import qualified Data.Text           as T

import           Control.Monad.Extra (pureIf)
import           Data.Char           (isAscii)
import qualified Data.Map            as M
import           Data.These          (These (..))
import           Intlc.Core
import           Intlc.ICU
import           Prelude

data ExternalLint
  = RedundantSelect
  deriving (Eq, Show)

data InternalLint
  = TooManyInterpolations
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

type Rule a = Stream -> Maybe a

lintWith :: [Rule a] -> Message -> Status a
lintWith rules (Message stream) = toStatus $ rules `flap` stream
  where toStatus = maybeToStatus . nonEmpty . catMaybes

lintExternal :: Message -> Status ExternalLint
lintExternal = lintWith
  [ redundantSelectRule
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
lintDatasetExternal = lintDatasetWith lintExternal formatExternalFailure

lintDatasetInternal :: Dataset Translation -> Maybe Text
lintDatasetInternal = lintDatasetWith lintInternal formatInternalFailure

formatFailureWith :: (Functor f, Foldable f) => (a -> Text) -> Text -> f a -> Text
formatFailureWith f k es = title <> msgs
  where title = k <> ": \n"
        msgs = T.intercalate "\n" . toList . fmap (indent . f) $ es
        indent = (" " <>)

formatExternalFailure :: (Functor f, Foldable f) => Text -> f ExternalLint -> Text
formatExternalFailure = formatFailureWith $ \case
  RedundantSelect -> "Redundant select found"

formatInternalFailure :: (Functor f, Foldable f) => Text -> f InternalLint -> Text
formatInternalFailure = formatFailureWith $ \case
  TooManyInterpolations            -> "Nested functions are not allowed"
  (InvalidNonAsciiCharacter chars) -> "Following characters are not allowed: " <> intercalateChars chars
    where intercalateChars:: NonEmpty Char -> Text
          intercalateChars = T.intercalate " " . toList . fmap T.singleton

redundantSelectRule :: Rule ExternalLint
redundantSelectRule []     = Nothing
redundantSelectRule (x:xs)
  | isRedundant x = Just RedundantSelect
  | otherwise     = redundantSelectRule xs
  -- If there's only a wildcard it could have been a plain string instead.
  where isRedundant (Interpolation _ (Select (That _w))) = True
        isRedundant _                                    = False

interpolationsRule :: Rule InternalLint
interpolationsRule = go 0
  where
    go :: Int -> Rule InternalLint
    go 2 _      = Just TooManyInterpolations
    go _ []     = Nothing
    go n (x:xs) = go n' $ maybeToMonoid mys <> xs
      where
        mys = getStream' x
        n' = n + length mys
        -- We can count streams to understand how often interpolations
        -- occur, however we exclude callbacks and plurals from this. The
        -- former because the vendor's tool has no issues parsing its syntax
        -- and the latter because it's a special case that we can't rewrite.
        getStream' (Interpolation _ (Callback {})) = Nothing
        getStream' (Interpolation _ (Plural {}))   = Nothing
        getStream' token                           = getStream token

-- Allows any ASCII character as well as a handful of Unicode characters that
-- we've established are safe for use with our vendor's tool.
isAcceptedChar :: Char -> Bool
isAcceptedChar c = isAscii c || c `elem` acceptedChars
  where acceptedChars = ['’','…','é','—','ƒ','“','”','–']

unsupportedUnicodeRule :: Rule InternalLint
unsupportedUnicodeRule = output . nonAscii where
  output = fmap InvalidNonAsciiCharacter . nonEmpty . T.unpack
  nonAscii :: Stream -> Text
  nonAscii []                      = mempty
  nonAscii (Plaintext x:ys)        = T.filter (not . isAcceptedChar) x <> nonAscii ys
  nonAscii (x@Interpolation {}:ys) = nonAscii (maybeToMonoid . getStream $ x) <> nonAscii ys
