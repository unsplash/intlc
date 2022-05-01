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

-- We diverge from icu4j by supporting a boolean type, and not necessarily
-- requiring wildcard cases.
data Type
  = Bool { trueCase :: Stream, falseCase :: Stream }
  | String
  | Number (Maybe NumberSkeleton)
  | Date DateTimeFmt
  | Time DateTimeFmt
  | Plural Plural
  -- Plural hash references have their own distinct type rather than merely
  -- taking on `Number` to allow compilers to infer appropriately.
  | PluralRef
  | Select (NonEmpty SelectCase) (Maybe SelectWildcard)
  | Callback Stream
  deriving (Show, Eq)

-- Only a small subset of the ICU number skeleton spec is currently
-- implemented. This can be extended at any time.
data NumberSkeleton = NumberSkeleton NumberToken
  deriving (Show, Eq)

data NumberToken
  = Currency CurrencyCode
  | Measure NumberMeasureUnit
  | Percent
  deriving (Show, Eq)

-- For now we're listing these explicitly so that we can check for typos in the
-- parser. At some point we may need to give up and replace this with unsafe
-- plaintext.
data CurrencyCode
  = USD
  | EUR
  | GBP
  | CNY
  | JPY
  | CAD
  deriving (Show, Read, Eq)

-- Refer to this list for units supported in JavaScript:
--   https://tc39.es/proposal-unified-intl-numberformat/section6/locales-currencies-tz_proposed_out.html#sec-issanctionedsimpleunitidentifier
data NumberMeasureUnit
  = Megabyte
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
