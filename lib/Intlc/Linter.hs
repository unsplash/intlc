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

statusToMaybe :: Status -> Maybe LintingError
statusToMaybe Success     = Nothing
statusToMaybe (Failure x) = Just x

interpolationsRule :: Stream -> Status
interpolationsRule = go 0
  where
    go :: Int -> Stream -> Status
    go 2 _      = Failure TooManyInterpolations
    go _ []     = Success
    go n (x:xs) = go n' $ maybeToMonoid mys <> xs
      where mys = getStream x
            n' = n + length mys

lint :: Message -> Status
lint (Message stream) = interpolationsRule stream
