module Intlc.Core where

import           Prelude

data ICUType
  = Number
  | Callback [Token]
  deriving (Show, Eq)

data Arg = Arg Text (Maybe ICUType)
  deriving (Show, Eq)

-- | A token is either an interpolation - some sort of identifier for input -
-- or mere plaintext. A collection of tokens make up any message. A message
-- without any interpolation will be a single `Plaintext` token.
data Token
  = Plaintext Text
  | Interpolation Arg
  deriving (Show, Eq)

data Message
  = Static Text
  | Dynamic [Token]
  deriving (Show, Eq)

type Dataset = Map Text
