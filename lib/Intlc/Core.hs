module Intlc.Core where

import           Prelude

-- | A token is either an interpolation - some sort of identifier for input -
-- or mere plaintext. A collection of tokens make up any translation. A
-- translation without any interpolation will be a single `Plaintext` token.
data Token
  = Plaintext Text
  | Interpolation Text
  deriving (Show, Eq)

data Translation
  = Static Text
  | Dynamic [Token]
  deriving (Show, Eq)
