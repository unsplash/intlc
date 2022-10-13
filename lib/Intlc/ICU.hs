-- This module defines an AST for ICU messages. We do not necessarily behave
-- identically to other implementations.

module Intlc.ICU where

import           Prelude hiding (Type)

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
  -- The only cardinal plurals which do not require a wildcard are those
  -- consisting solely of literal/exact cases. This is because within the AST we
  -- only care about correctness and prospective type safety, not optimal use of
  -- ICU syntax.
  --
  -- Ordinal plurals always require a wildcard as per their intended usage with
  -- rules, however as with the cardinal plural type we'll allow a wider set of
  -- suboptimal usages that we can then lint against.
  | CardinalExact Arg (NonEmpty (PluralCase PluralExact))
  | CardinalInexact Arg [PluralCase PluralExact] [PluralCase PluralRule] Stream
  | Ordinal Arg [PluralCase PluralExact] [PluralCase PluralRule] Stream
  -- Plural hash references have their own distinct type rather than merely
  -- taking on `Number` to allow compilers to infer appropriately.
  | PluralRef Arg
  | SelectNamed Arg (NonEmpty SelectCase)
  | SelectWild Arg Stream
  | SelectNamedWild Arg (NonEmpty SelectCase) Stream
  | Callback Arg Stream
  deriving (Show, Eq)

data DateTimeFmt
  = Short
  | Medium
  | Long
  | Full
  deriving (Show, Eq)

type PluralCase a = (a, Stream)

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

type SelectCase = (Text, Stream)

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
getNamedStream (CardinalExact n ls)        = Just (n, getPluralCaseStream `concatMap` ls)
getNamedStream (CardinalInexact n ls rs w) = Just . (n,) $ mconcat
  [ getPluralCaseStream `concatMap` ls
  , getPluralCaseStream `concatMap` rs
  , w
  ]
getNamedStream (Ordinal n xs ys w)         = Just . (n,) $ mconcat
  [ getPluralCaseStream `concatMap` xs
  , getPluralCaseStream `concatMap` ys
  , w
  ]
getNamedStream (SelectNamed n xs)        = Just (n, snd `concatMap` xs)
getNamedStream (SelectWild n xs)         = Just (n, xs)
getNamedStream (SelectNamedWild n xs ys) = Just (n, snd `concatMap` xs <> ys)
getNamedStream (Callback n xs) = Just (n, xs)

getPluralCaseStream :: PluralCase a -> Stream
getPluralCaseStream = snd
