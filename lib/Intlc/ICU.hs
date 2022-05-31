{-# LANGUAGE NamedFieldPuns #-}

-- This module defines an AST for ICU messages. We do not necessarily behave
-- identically to other implementations.

module Intlc.ICU where

import           Prelude hiding (Type)

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
getStream Plaintext {} = Nothing
getStream (Interpolation _ String)                                           = Nothing
getStream (Interpolation _ Number)                                           = Nothing
getStream (Interpolation _ Date {})                                          = Nothing
getStream (Interpolation _ Time {})                                          = Nothing
getStream (Interpolation _ PluralRef)                                        = Nothing
getStream (Interpolation _ Bool {trueCase, falseCase})                       = Just $ trueCase <> falseCase
-- TODO: plural cases are really complicated to pattern match, is there a better way to handle all of this?
getStream (Interpolation _ Plural {})                                        = Just []
getStream (Interpolation _ (Select case' Nothing))                           = Just $ concatMap (\(SelectCase _ xs) -> xs) case'
getStream (Interpolation _ (Select case' (Just (SelectWildcard wildcards)))) = Just $ wildcards <> concatMap (\(SelectCase _ xs) -> xs) case'
getStream (Interpolation _ (Callback xs))                                    = Just xs

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
