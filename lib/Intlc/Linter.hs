module Intlc.Linter where

import           Intlc.ICU
import           Prelude   hiding (Type)

data LintingError
  = TooManyInterpolations
  deriving (Eq, Show)

data Status
  = Success
  | Failure LintingError
  deriving (Eq, Show)

countInterpolations :: NEStream -> Int
countInterpolations = foldr go 0
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
    go :: Token -> Int -> Int
    go Plaintext {} n              = n
    go (Interpolation (Arg _ x)) n = count x + n

lint :: Message -> Status
lint Static {} = Success
lint (Dynamic stream) = (mkStatus . countInterpolations) stream
  where
    mkStatus n = if n > 1
      then Failure TooManyInterpolations
      else Success

