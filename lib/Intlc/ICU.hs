-- This module defines an AST for ICU messages. We do not necessarily behave
-- identically to other implementations.

module Intlc.ICU where

import           Prelude hiding (Type)

data Arg = Arg Text Type
  deriving (Show, Eq)

data Message
  = Static Text
  | Dynamic [Token]
  deriving (Show, Eq)

-- | A token is either an interpolation - some sort of identifier for input -
-- or mere plaintext. A collection of tokens make up any message. A message
-- without any interpolation will be a single `Plaintext` token.
data Token
  = Plaintext Text
  | Interpolation Arg
  deriving (Show, Eq)

data Type
  = String
  | Number
  | Date DateFmt
  | Plural (NonEmpty PluralCase) PluralWildcard
  -- We diverge from icu4j by not requiring a wildcard case.
  | Select (NonEmpty SelectCase) (Maybe SelectWildcard)
  | Callback [Token]
  deriving (Show, Eq)

data DateFmt
  = Short
  | Medium
  | Long
  | Full
  deriving (Show, Eq)

data SelectCase = SelectCase Text [Token]
  deriving (Show, Eq)

newtype SelectWildcard = SelectWildcard [Token]
  deriving (Show, Eq)

-- `Text` here is our count. It's represented as a string so that we can dump
-- it back out without thinking about converting numeric types across languages.
data PluralCase = PluralCase Text [Token]
  deriving (Show, Eq)

newtype PluralWildcard = PluralWildcard [Token]
  deriving (Show, Eq)
