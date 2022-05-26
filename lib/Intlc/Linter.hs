module Intlc.Linter where

import           Data.List.NonEmpty ((!!))
import           Intlc.ICU
import           Prelude            hiding (Type)

data LintingError
  = TooManyInterpolations
  deriving (Eq, Show)

data Status
  = Success
  | Failure LintingError
  deriving (Eq, Show)

hasManyInterpolations :: NEStream -> Int -> Int -> Bool
hasManyInterpolations xs i n
  | n > 1 = True
  | i < length xs = hasManyInterpolations xs (i+1) $ result (xs !! i)
  | otherwise = False
  where
    result :: Token -> Int
    result Plaintext {}              = n
    result (Interpolation (Arg _ x)) = count x + n
      where
        count = \case
            String      -> 0
            Number      -> 0
            Date {}     -> 0
            Time {}     -> 0
            PluralRef   -> 0
            Bool {}     -> 1
            Plural {}   -> 1
            Select {}   -> 1
            Callback {} -> 1


lint :: Message -> Status
lint Static {} = Success
lint (Dynamic stream)
  | hasManyInterpolations stream 0 0 = Failure TooManyInterpolations
  | otherwise = Success

