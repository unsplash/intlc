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

isFailure :: Status -> Bool
isFailure Failure {} = True
isFailure _          = False

interpolationsRule :: Stream -> Status
interpolationsRule = result 0

  where
    result :: Int -> Stream -> Status
    result 2 _      = Failure TooManyInterpolations
    result _ []     = Success
    result n (x:xs) = case exit' x n of
      (n', ys) -> result n' $ ys <> xs

    exit' :: Token -> Int -> (Int, Stream)
    exit' Plaintext {} n = (n, [])
    exit' (Interpolation _ String) n                                           = (n, [])
    exit' (Interpolation _ Number) n                                           = (n, [])
    exit' (Interpolation _ Date {}) n                                          = (n, [])
    exit' (Interpolation _ Time {}) n                                          = (n, [])
    exit' (Interpolation _ PluralRef) n                                        = (n, [])
    exit' (Interpolation _ Bool {trueCase, falseCase}) n                       = (n + 1, trueCase <> falseCase)

    -- TODO: plural cases are really complicated to pattern match, is there a better way to handle all of this?
    exit' (Interpolation _ Plural {}) n                                        = (n, [])
    exit' (Interpolation _ (Select case' Nothing)) n                           = (n + 1, concatMap (\(SelectCase _ xs) -> xs) case')
    exit' (Interpolation _ (Select case' (Just (SelectWildcard wildcards)))) n = (n + 1, wildcards <> concatMap (\(SelectCase _ xs) -> xs) case')
    exit' (Interpolation _ (Callback xs)) n                                    = (n + 1, xs)

lint :: Message -> Status
lint (Message stream) = interpolationsRule stream
