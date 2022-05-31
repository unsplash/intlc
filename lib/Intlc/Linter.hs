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
    go n (x:xs) = go n' $ maybeToMonoid mys <> xs
      where mys = getStream x
            n' = n + length mys

    getStream :: Token -> Maybe Stream
    getStream Plaintext {} = Nothing
    getStream (Interpolation _ String)                                           = Nothing
    getStream (Interpolation _ Number)                                           = Nothing
    getStream (Interpolation _ Date {})                                          = Nothing
    getStream (Interpolation _ Time {})                                          = Nothing
    getStream (Interpolation _ PluralRef)                                        = Nothing
    getStream (Interpolation _ Bool {trueCase, falseCase})                       = Just $ trueCase <> falseCase

    -- TODO: plural cases are really complicated to pattern match, is there a better way to handle all of this?
    getStream (Interpolation _ Plural {})                                        = Just []
    getStream (Interpolation _ (Select case' Nothing))                           = Just $ concatMap (\(SelectCase _ xs) -> xs) case'
    getStream (Interpolation _ (Select case' (Just (SelectWildcard wildcards)))) = Just $ wildcards <> concatMap (\(SelectCase _ xs) -> xs) case'
    getStream (Interpolation _ (Callback xs))                                    = Just xs

lint :: Message -> Status
lint (Message stream) = interpolationsRule stream
