-- This module defines an AST for ICU messages. We do not necessarily behave
-- identically to other implementations.

module Intlc.ICU where

import           Data.These (These (..))
import           Prelude    hiding (Type)

newtype Message = Message Stream
  deriving (Show, Eq)

unMessage :: Message -> Stream
unMessage (Message xs) = xs

type Stream = [Node]

newtype Arg = Arg Text
  deriving (Show, Eq, Ord, IsString)

unArg :: Arg -> Text
unArg (Arg x) = x

-- | A `Node` is either an interpolation - some sort of identifier for input -
-- or mere plaintext. A collection of nodes make up any message. A non-empty
-- message without any interpolation will be a single `Plaintext` node.
--
-- On interpolations we diverge from icu4j by supporting a boolean type, and
-- not necessarily requiring wildcard cases.
data Node
  = Plaintext Text
  | Bool { name :: Arg, trueCase :: Stream, falseCase :: Stream }
  | String Arg
  | Number Arg
  | Date Arg DateTimeFmt
  | Time Arg DateTimeFmt
  | Plural Arg Plural
  -- Plural hash references have their own distinct type rather than merely
  -- taking on `Number` to allow compilers to infer appropriately.
  | PluralRef Arg
  | Select Arg (These (NonEmpty SelectCase) SelectWildcard)
  | Callback Arg Stream
  deriving (Show, Eq)

data DateTimeFmt
  = Short
  | Medium
  | Long
  | Full
  deriving (Show, Eq)

-- | The only cardinal plurals which do not require a wildcard are those
-- consisting solely of literal/exact cases. This is because within the AST we
-- only care about correctness and prospective type safety, not optimal use of
-- ICU syntax.
--
-- Ordinal plurals always require a wildcard as per their intended usage with
-- rules, however as with the cardinal plural type we'll allow a wider set of
-- suboptimal usages that we can then lint against.
data Plural
  = CardinalExact (NonEmpty (PluralCase PluralExact))
  | CardinalInexact [PluralCase PluralExact] [PluralCase PluralRule] PluralWildcard
  | Ordinal [PluralCase PluralExact] [PluralCase PluralRule] PluralWildcard
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

-- | Merges any sibling `Plaintext` nodes in a `Stream`.
mergePlaintext :: Stream -> Stream
mergePlaintext []                               = []
mergePlaintext (Plaintext x : Plaintext y : zs) = mergePlaintext $ Plaintext (x <> y) : zs
mergePlaintext (x:ys)                           = x : mergePlaintext ys

getStream :: Node -> Maybe Stream
getStream = fmap snd . getNamedStream

getNamedStream :: Node -> Maybe (Arg, Stream)
getNamedStream Plaintext {}    = Nothing
getNamedStream String {}       = Nothing
getNamedStream Number {}       = Nothing
getNamedStream Date {}         = Nothing
getNamedStream Time {}         = Nothing
getNamedStream PluralRef {}    = Nothing
getNamedStream x@(Bool {})     = Just (name x, trueCase x <> falseCase x)
getNamedStream (Plural n x)    = Just (n, getPluralStream x)
getNamedStream (Select n x)    = Just . (n,) . bifoldMap (concatMap f) g $ x
    where f (SelectCase _ xs)  = xs
          g (SelectWildcard w) = w
getNamedStream (Callback n xs) = Just (n, xs)

getPluralStream :: Plural -> Stream
getPluralStream (CardinalExact ls)        = getPluralCaseStream `concatMap` ls
getPluralStream (CardinalInexact ls rs w) = mconcat
  [ getPluralCaseStream `concatMap` ls
  , getPluralCaseStream `concatMap` rs
  , getPluralWildcardStream w
  ]
getPluralStream (Ordinal xs ys w)         = mconcat
  [ getPluralCaseStream `concatMap` xs
  , getPluralCaseStream `concatMap` ys
  , getPluralWildcardStream w
  ]

getPluralCaseStream :: PluralCase a -> Stream
getPluralCaseStream (PluralCase _ xs) = xs

getPluralWildcardStream :: PluralWildcard -> Stream
getPluralWildcardStream (PluralWildcard xs) = xs
