module Intlc.Linter where

import           Intlc.ICU
import           Prelude   hiding (Type)

data InternalLintingError
  = TooManyInterpolations
  deriving (Eq, Show)

data Status
  = Success
  | Failure (NonEmpty InternalLintingError)
  deriving (Eq, Show)

statusToMaybe :: Status -> Maybe (NonEmpty InternalLintingError)
statusToMaybe Success      = Nothing
statusToMaybe (Failure xs) = Just xs

maybeToStatus :: Maybe (NonEmpty InternalLintingError) -> Status
maybeToStatus Nothing   = Success
maybeToStatus (Just xs) = Failure xs

interpolationsRule :: Stream -> Maybe InternalLintingError
interpolationsRule = go 0
  where
    go :: Int -> Stream -> Maybe InternalLintingError
    go 2 _      = Just TooManyInterpolations
    go _ []     = Nothing
    go n (x:xs) = go n' $ maybeToMonoid mys <> xs
      where mys = getStream x
            n' = n + length mys

lint :: Message -> Status
lint (Message stream) = toStatus $ rules `flap` stream
  where toStatus = maybeToStatus . nonEmpty . catMaybes
        rules = [interpolationsRule]
