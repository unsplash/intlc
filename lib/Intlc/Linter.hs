module Intlc.Linter where

import           Intlc.ICU
import           Prelude

data InternalLint
  = TooManyInterpolations
  deriving (Eq, Show)

data Status a
  = Success
  | Failure (NonEmpty a)
  deriving (Eq, Show)

type Rule a = Stream -> Maybe a

statusToMaybe :: Status a -> Maybe (NonEmpty a)
statusToMaybe Success      = Nothing
statusToMaybe (Failure xs) = Just xs

maybeToStatus :: Maybe (NonEmpty a) -> Status a
maybeToStatus Nothing   = Success
maybeToStatus (Just xs) = Failure xs

interpolationsRule :: Rule InternalLint
interpolationsRule = go 0
  where
    go :: Int -> Rule InternalLint
    go 2 _      = Just TooManyInterpolations
    go _ []     = Nothing
    go n (x:xs) = go n' $ maybeToMonoid mys <> xs
      where mys = getStream x
            n' = n + length mys

lintWith :: [Rule a] -> Message -> Status a
lintWith rules (Message stream) = toStatus $ rules `flap` stream
  where toStatus = maybeToStatus . nonEmpty . catMaybes

lintInternal :: Message -> Status InternalLint
lintInternal = lintWith
  [ interpolationsRule
  ]
