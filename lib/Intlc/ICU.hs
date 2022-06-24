{-# LANGUAGE NamedFieldPuns #-}

-- This module defines an AST for ICU messages. We do not necessarily behave
-- identically to other implementations.

module Intlc.ICU where

import           Data.These (These (..), mergeTheseWith)
import           Prelude    hiding (Type)

newtype Message = Message Stream
  deriving (Show, Eq)

unMessage :: Message -> Stream
unMessage (Message xs) = xs

type Stream = [Token]

-- | A token is either an interpolation - some sort of identifier for input -
-- or mere plaintext. A collection of tokens make up any message. A non-empty
-- message without any interpolation will be a single `Plaintext` token.
data Token
  = Plaintext Text
  | Interpolation Text Type
  deriving (Show, Eq)

-- | Merges any sibling `Plaintext` tokens in a `Stream`.
mergePlaintext :: Stream -> Stream
mergePlaintext []                               = []
mergePlaintext (Plaintext x : Plaintext y : zs) = mergePlaintext $ Plaintext (x <> y) : zs
mergePlaintext (x:ys)                           = x : mergePlaintext ys

getStream :: Token -> Maybe Stream
getStream Plaintext {}        = Nothing
getStream (Interpolation _ t) = case t of
  String                     -> Nothing
  Number                     -> Nothing
  Date {}                    -> Nothing
  Time {}                    -> Nothing
  PluralRef                  -> Nothing
  Bool {trueCase, falseCase} -> Just $ trueCase <> falseCase
  Plural x                   -> Just $ getPluralStream x
  Select x                   -> Just . mergeTheseWith (concatMap f) g (<>) $ x
    where f (SelectCase _ xs)  = xs
          g (SelectWildcard w) = w
  Callback xs                -> Just xs

getPluralStream :: Plural -> Stream
getPluralStream (Cardinal x) = getCardinalStream x
getPluralStream (Ordinal x)  = getOrdinalStream x

getCardinalStream :: CardinalPlural -> Stream
getCardinalStream (LitPlural xs mw) = join
  [ getPluralCaseStream `concatMap` xs
  , maybeToMonoid $ getPluralWildcardStream <$> mw
  ]
getCardinalStream (RulePlural xs w) = join
  [ getPluralCaseStream `concatMap` xs
  , getPluralWildcardStream w
  ]
getCardinalStream (MixedPlural xs ys w) = join
  [ getPluralCaseStream `concatMap` xs
  , getPluralCaseStream `concatMap` ys
  , getPluralWildcardStream w
  ]

getOrdinalStream :: OrdinalPlural -> Stream
getOrdinalStream (OrdinalPlural xs ys w) = join
  [ getPluralCaseStream `concatMap` xs
  , getPluralCaseStream `concatMap` ys
  , getPluralWildcardStream w
  ]

getPluralCaseStream :: PluralCase a -> Stream
getPluralCaseStream (PluralCase _ xs) = xs

getPluralWildcardStream :: PluralWildcard -> Stream
getPluralWildcardStream (PluralWildcard xs) = xs

-- We diverge from icu4j by supporting a boolean type, and not necessarily
-- requiring wildcard cases.
data Type
  = Bool { trueCase :: Stream, falseCase :: Stream }
  | String
  | Number
  | Date DateTimeFmt
  | Time DateTimeFmt
  | Plural Plural
  -- Plural hash references have their own distinct type rather than merely
  -- taking on `Number` to allow compilers to infer appropriately.
  | PluralRef
  | Select (These (NonEmpty SelectCase) SelectWildcard)
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
