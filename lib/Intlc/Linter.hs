module Intlc.Linter where

import           Intlc.ICU
import           Prelude   hiding (Type)

data Status
  = Success
  | Failure Text

count :: Type -> Int
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

countInterpolations :: NEStream -> Int
countInterpolations = foldr go 0
  where
    go :: Token -> Int -> Int
    go t n = case t of
      Plaintext {}              -> n
      Interpolation ((Arg _ y)) -> count y + n

lint :: Message -> Status
lint x = case x of
  Static _                              -> Success
  Dynamic neStream -> (mkStatus . (> 2) . countInterpolations) neStream
    where
      mkStatus = \case
        True  -> Failure "Too many interpolations"
        False -> Success

