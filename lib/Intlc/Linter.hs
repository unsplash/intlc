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

data InterpolationExit
  = Stop
  | Continue Int

isFailure :: Status -> Bool
isFailure Failure {} = True
isFailure _          = False

interpolationsRule :: Stream -> Status
interpolationsRule s = case result 0 s of
  Continue {} -> Success
  Stop        -> Failure TooManyInterpolations

  where
    result :: Int -> Stream -> InterpolationExit
    result 2 _      = Stop
    result n []     = Continue n
    result n (x:xs) = case exit' x of
      Continue inner -> result (inner + n) xs
      Stop           -> Stop

    exit' :: Token -> InterpolationExit
    exit' Plaintext {} = Continue 0
    exit' (Interpolation (Arg _ String))                                           = Continue 0
    exit' (Interpolation (Arg _ Number))                                           = Continue 0
    exit' (Interpolation (Arg _ Date {}))                                          = Continue 0
    exit' (Interpolation (Arg _ Time {}))                                          = Continue 0
    exit' (Interpolation (Arg _ PluralRef))                                        = Continue 0
    exit' (Interpolation (Arg _ Bool {trueCase=trueXs, falseCase=falseXs}))        = (result 1 . (trueXs <>)) falseXs

    -- TODO: plural cases are really complicated to pattern match, is there a better way to handle all of this?
    exit' (Interpolation (Arg _ Plural {}))                                        = Continue 1
    exit' (Interpolation (Arg _ (Select case' Nothing)))                           = (result 1 . concatMap (\(SelectCase _ xs) -> xs)) case'
    exit' (Interpolation (Arg _ (Select case' (Just (SelectWildcard wildcards))))) = (result 1 . (wildcards <>) . concatMap (\(SelectCase _ xs) -> xs)) case'
    exit' (Interpolation (Arg _ (Callback xs)))                                    = result 1 xs





lint :: Message -> Status
lint Static {}        = Success
lint (Dynamic stream) = interpolationsRule (toList stream)

