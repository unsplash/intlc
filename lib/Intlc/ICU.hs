-- This module defines an AST for ICU messages. We do not necessarily behave
-- identically to other implementations.

module Intlc.ICU where

import           Prelude hiding (Type)

data Message
  = Static Text
  | Dynamic NEStream
  deriving (Show, Eq)

type NEStream = NonEmpty Token
type Stream = [Token]

-- | A token is either an interpolation - some sort of identifier for input -
-- or mere plaintext. A collection of tokens make up any message. A non-empty
-- message without any interpolation will be a single `Plaintext` token.
data Token
  = Plaintext Text
  | Interpolation Arg
  deriving (Show, Eq)

-- | Merges any sibling `Plaintext` tokens in a `Stream`.
mergePlaintext :: Stream -> Stream
mergePlaintext []                               = []
mergePlaintext (Plaintext x : Plaintext y : zs) = mergePlaintext $ Plaintext (x <> y) : zs
mergePlaintext (x:ys)                           = x : mergePlaintext ys

data Arg = Arg Text Type
  deriving (Show, Eq)

-- We diverge from icu4j by not necessarily requiring wildcard cases.
data Type
  = String
  | Number
  | Date DateTimeFmt
  | Time DateTimeFmt
  | Plural Plural
  | Select (NonEmpty SelectCase) (Maybe SelectWildcard)
  | Callback Stream
  deriving (Show, Eq)

data DateTimeFmt
  = Short
  | Medium
  | Long
  | Full
  deriving (Show, Eq)

data Plural
  = Cardinal CardinalPlural
  | Ordinal OrdinalPlural
  deriving (Show, Eq)

-- | Cardinal plurals can be split into four usages:
--
--   1. Literal number cases without a wildcard.
--   2. Literal number cases with a wildcard.
--   3. Rule cases with a wildcard.
--   3. Mixed cases with a wildcard.
--
-- Per the aforementioned usages, any cardinal plural with at least one rule
-- case must have a wildcard, and cardinal plurals without any rule cases can
-- optionally supply a wildcard.
data CardinalPlural
  = LitPlural (NonEmpty (PluralCase PluralExact)) (Maybe PluralWildcard)
  | RulePlural (NonEmpty (PluralCase PluralRule)) PluralWildcard
  | MixedPlural (NonEmpty (PluralCase PluralExact)) (NonEmpty (PluralCase PluralRule)) PluralWildcard
  deriving (Show, Eq)

-- | Ordinal plurals require at least one rule case and therefore also a
-- wildcard. An ordinal plural without a rule case would make the use of this
-- construct redundant, and in such cases the consumer should instead use a
-- cardinal plural.
data OrdinalPlural
  = OrdinalPlural [PluralCase PluralExact] (NonEmpty (PluralCase PluralRule)) PluralWildcard
  deriving (Show, Eq)

data PluralCase a = PluralCase a Stream
  deriving (Show, Eq)

-- `Text` here is our count. It's represented as a string so that we can dump
-- it back out without thinking about converting numeric types across
-- languages.
newtype PluralExact = PluralExact Text
  deriving (Show, Eq)

-- "Other" is implied in the wildcard.
data PluralRule
  = Zero
  | One
  | Two
  | Few
  | Many
  deriving (Show, Eq)

newtype PluralWildcard = PluralWildcard Stream
  deriving (Show, Eq)

data SelectCase = SelectCase Text Stream
  deriving (Show, Eq)

newtype SelectWildcard = SelectWildcard Stream
  deriving (Show, Eq)
