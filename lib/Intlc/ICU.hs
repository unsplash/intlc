-- This module defines an AST for ICU messages. We do not necessarily behave
-- identically to other implementations.

module Intlc.ICU where

import           Prelude hiding (Type)

data Message
  = Static Text
  | Dynamic Stream
  deriving (Show, Eq)

type Stream = [Token]

-- | A token is either an interpolation - some sort of identifier for input -
-- or mere plaintext. A collection of tokens make up any message. A message
-- without any interpolation will be a single `Plaintext` token.
data Token
  = Plaintext Text
  | Interpolation Arg
  deriving (Show, Eq)

data Arg = Arg Text Type
  deriving (Show, Eq)

data Type
  = String
  | Number
  | Date DateFmt
  | Plural (NonEmpty PluralCase) PluralWildcard
  -- We diverge from icu4j by not requiring a wildcard case.
  | Select (NonEmpty SelectCase) (Maybe SelectWildcard)
  | Callback Stream
  deriving (Show, Eq)

data DateFmt
  = Short
  | Medium
  | Long
  | Full
  deriving (Show, Eq)

data SelectCase = SelectCase Text Stream
  deriving (Show, Eq)

newtype SelectWildcard = SelectWildcard Stream
  deriving (Show, Eq)

-- `Text` here is our count. It's represented as a string so that we can dump
-- it back out without thinking about converting numeric types across languages.
data PluralCase = PluralCase Text Stream
  deriving (Show, Eq)

newtype PluralWildcard = PluralWildcard Stream
  deriving (Show, Eq)
