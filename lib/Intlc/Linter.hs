module Intlc.Linter where

import qualified Data.Text  as T

import           Data.Char  (isAscii)
import           Data.These (These (..))
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
    go n (x:xs) = go (n' x) $ maybeToMonoid (mys x) <> xs
      where
        mys (Interpolation _ (Callback {})) = Nothing
        mys (Interpolation _ (Plural {}))   = Nothing
        mys token                           = getStream token
        n' token = n + length (mys token)

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

formatLintingError :: InternalLint -> Text
formatLintingError TooManyInterpolations            = "Nested functions are not allowed"
formatLintingError (InvalidNonAsciiCharacter chars) = "Following characters are not allowed: " <> intercalateChars chars
  where intercalateChars:: NonEmpty Char -> Text
        intercalateChars = T.intercalate " " . toList . fmap T.singleton
