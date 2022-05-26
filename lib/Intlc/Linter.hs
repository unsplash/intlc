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
    go t n = case t of
      Plaintext {}              -> n
      Interpolation ((Arg _ x)) -> count x + n

lint :: Message -> Status
lint m = case m of
  Static {}        -> Success
  Dynamic neStream -> (mkStatus . countInterpolations) neStream
    where
      mkStatus n = if n > 1
        then Failure TooManyInterpolations
        else Success

