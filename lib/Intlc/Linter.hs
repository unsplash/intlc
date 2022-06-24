module Intlc.Linter where

import           Data.These (These (..))
import           Intlc.ICU
import           Prelude

data ExternalLint
  = RedundantSelect
  deriving (Eq, Show)

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

redundantSelectRule :: Rule ExternalLint
redundantSelectRule []     = Nothing
redundantSelectRule (x:xs)
  | isRedundant x = Just RedundantSelect
  | otherwise     = redundantSelectRule xs
  -- If there's only a wildcard it could have been a plain string instead.
  where isRedundant (Interpolation _ (Select (That _w))) = True
        isRedundant _                                    = False

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

lintExternal :: Message -> Status ExternalLint
lintExternal = lintWith
  [ redundantSelectRule
  ]

lintInternal :: Message -> Status InternalLint
lintInternal = lintWith
  [ interpolationsRule
  ]
