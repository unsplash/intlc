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
interpolationsRule = go 0
  where
    go :: Int -> Stream -> Status
    go 2 _      = Failure TooManyInterpolations
    go _ []     = Success
    go n (x:xs) = go n' $ ys <> xs
      where (n', ys) = getStream x n

    getInterps :: Token -> Int -> (Int, Stream)
    getInterps Plaintext {} n = (n, [])
    getInterps (Interpolation _ String) n                                           = (n, [])
    getInterps (Interpolation _ Number) n                                           = (n, [])
    getInterps (Interpolation _ Date {}) n                                          = (n, [])
    getInterps (Interpolation _ Time {}) n                                          = (n, [])
    getInterps (Interpolation _ PluralRef) n                                        = (n, [])
    getInterps (Interpolation _ Bool {trueCase, falseCase}) n                       = (n + 1, trueCase <> falseCase)

    -- TODO: plural cases are really complicated to pattern match, is there a better way to handle all of this?
    getInterps (Interpolation _ Plural {}) n                                        = (n, [])
    getInterps (Interpolation _ (Select case' Nothing)) n                           = (n + 1, concatMap (\(SelectCase _ xs) -> xs) case')
    getInterps (Interpolation _ (Select case' (Just (SelectWildcard wildcards)))) n = (n + 1, wildcards <> concatMap (\(SelectCase _ xs) -> xs) case')
    getInterps (Interpolation _ (Callback xs)) n                                    = (n + 1, xs)

lint :: Message -> Status
lint (Message stream) = interpolationsRule stream
