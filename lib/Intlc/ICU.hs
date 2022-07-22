-- This module defines an AST for ICU messages. We do not necessarily behave
-- identically to other implementations.

module Intlc.ICU where

import           Data.These (These (..))
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

-- | The only cardinal plurals which do not require a wildcard are those
-- consisting solely of literal/exact cases. This is because within the AST we
-- only care about correctness and prospective type safety, not optimal use of
-- ICU syntax.
data CardinalPlural
  = CardinalExact (NonEmpty (PluralCase PluralExact))
  | CardinalInexact [PluralCase PluralExact] [PluralCase PluralRule] PluralWildcard
  deriving (Show, Eq)

-- | Ordinal plurals always require a wildcard as per their intended usage with
-- rules, however as with the cardinal plural type we'll allow a wider set of
-- suboptimal usages that we can then lint against.
data OrdinalPlural
  = OrdinalPlural [PluralCase PluralExact] [PluralCase PluralRule] PluralWildcard
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
  deriving (Show, Eq, Ord, Enum, Bounded)

newtype PluralWildcard = PluralWildcard Stream
  deriving (Show, Eq)

data SelectCase = SelectCase Text Stream
  deriving (Show, Eq)

newtype SelectWildcard = SelectWildcard Stream
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
  Select x                   -> Just . bifoldMap (concatMap f) g $ x
    where f (SelectCase _ xs)  = xs
          g (SelectWildcard w) = w
  Callback xs                -> Just xs

getPluralStream :: Plural -> Stream
getPluralStream (Cardinal x) = getCardinalStream x
getPluralStream (Ordinal x)  = getOrdinalStream x

getCardinalStream :: CardinalPlural -> Stream
getCardinalStream (CardinalExact ls)        = getPluralCaseStream `concatMap` ls
getCardinalStream (CardinalInexact ls rs w) = mconcat
  [ getPluralCaseStream `concatMap` ls
  , getPluralCaseStream `concatMap` rs
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
