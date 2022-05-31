{-# LANGUAGE NamedFieldPuns #-}

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
    result n (x:xs) = case exit' x n of
      Continue n' -> result n' xs
      Stop        -> Stop

    exit' :: Token -> Int -> InterpolationExit
    exit' Plaintext {} n = Continue n
    exit' (Interpolation _ String) n                                           = Continue n
    exit' (Interpolation _ Number) n                                           = Continue n
    exit' (Interpolation _ Date {}) n                                          = Continue n
    exit' (Interpolation _ Time {}) n                                          = Continue n
    exit' (Interpolation _ PluralRef) n                                        = Continue n
    exit' (Interpolation _ Bool {trueCase, falseCase}) n                       = result (n + 1) $ trueCase <> falseCase

    -- TODO: plural cases are really complicated to pattern match, is there a better way to handle all of this?
    exit' (Interpolation _ Plural {}) n                                        = Continue n
    exit' (Interpolation _ (Select case' Nothing)) n                           = (result (n + 1) . concatMap (\(SelectCase _ xs) -> xs)) case'
    exit' (Interpolation _ (Select case' (Just (SelectWildcard wildcards)))) n = (result (n + 1) . (wildcards <>) . concatMap (\(SelectCase _ xs) -> xs)) case'
    exit' (Interpolation _ (Callback xs)) n                                    = result (n + 1) xs

lint :: Message -> Status
lint (Message stream) = interpolationsRule stream
