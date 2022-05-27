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

isFailure :: Status -> Bool
isFailure Failure {} = True
isFailure _          = False

interpolationsRule :: Stream -> Status
interpolationsRule = result 0
  where
    result :: Int -> Stream -> Status
    result 2 _      = Failure TooManyInterpolations
    result _ []     = Success
    result n (x:xs) = result (count x + n) xs


    count :: Token -> Int
    count = \case
      Plaintext {}                      -> 0
      Interpolation (Arg _ String)      -> 0
      Interpolation (Arg _ Number)      -> 0
      Interpolation (Arg _ Date {})     -> 0
      Interpolation (Arg _ Time {})     -> 0
      Interpolation (Arg _ PluralRef)   -> 0
      Interpolation (Arg _ Bool {})     -> 1
      Interpolation (Arg _ Plural {})   -> 1
      Interpolation (Arg _ Select {})   -> 1
      Interpolation (Arg _ Callback {}) -> 1



lint :: Message -> Status
lint Static {}        = Success
lint (Dynamic stream) = interpolationsRule (toList stream)

