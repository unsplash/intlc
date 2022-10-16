-- This module defines an AST for ICU messages. We do not necessarily behave
-- identically to other implementations.
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TypeFamilies       #-}

module Intlc.ICU where

import           Data.Functor.Foldable (Base, Corecursive, Recursive)
import qualified Data.Text             as T
import           Prelude

newtype Message = Message Node
  deriving (Show, Eq)

unMessage :: Message -> Node
unMessage (Message xs) = xs

newtype Arg = Arg Text
  deriving newtype (Show, Eq, Ord, IsString)

unArg :: Arg -> Text
unArg (Arg x) = x

-- | A `Node` is either an interpolation - some sort of identifier for input -
-- or mere plaintext. A collection of nodes make up any message. The entire AST
-- is represented as a single recursive node with the trailing `Node` always
-- representing the following sibling. Termination is represented by `Fin`,
-- equivalent to a list's `Nil`.
--
-- On interpolations we diverge from icu4j by supporting a boolean type, and
-- not necessarily requiring wildcard cases.
data Node
  = Fin
  | Char Char Node
  | Bool { name :: Arg, trueCase :: Node, falseCase :: Node, next :: Node }
  | String Arg Node
  | Number Arg Node
  | Date Arg DateTimeFmt Node
  | Time Arg DateTimeFmt Node
  -- The only cardinal plurals which do not require a wildcard are those
  -- consisting solely of literal/exact cases. This is because within the AST we
  -- only care about correctness and prospective type safety, not optimal use of
  -- ICU syntax.
  --
  -- Ordinal plurals always require a wildcard as per their intended usage with
  -- rules, however as with the cardinal plural type we'll allow a wider set of
  -- suboptimal usages that we can then lint against.
  | CardinalExact Arg (NonEmpty (PluralCase PluralExact)) Node
  | CardinalInexact Arg [PluralCase PluralExact] [PluralCase PluralRule] Node Node
  | Ordinal Arg [PluralCase PluralExact] [PluralCase PluralRule] Node Node
  -- Plural hash references have their own distinct type rather than merely
  -- taking on `Number` to allow compilers to infer appropriately.
  | PluralRef Arg Node
  | SelectNamed Arg (NonEmpty SelectCase) Node
  | SelectWild Arg Node Node
  | SelectNamedWild Arg (NonEmpty SelectCase) Node Node
  | Callback Arg Node Node
  deriving (Show, Eq, Generic, Recursive, Corecursive)

-- | A "pattern functor" representation of `Node`. Useful for recursion schemes
-- and pairing additional data to nodes. The two coexist distinctly so that the
-- most common use case, plain `Node`, can define a semigroup instance.
data NodeF a
  = FinF
  | CharF Char a
  | BoolF { nameF :: Arg, trueCaseF :: a, falseCaseF :: a, nextF :: a }
  | StringF Arg a
  | NumberF Arg a
  | DateF Arg DateTimeFmt a
  | TimeF Arg DateTimeFmt a
  -- The only cardinal plurals which do not require a wildcard are those
  -- consisting solely of literal/exact cases. This is because within the AST we
  -- only care about correctness and prospective type safety, not optimal use of
  -- ICU syntax.
  --
  -- Ordinal plurals always require a wildcard as per their intended usage with
  -- rules, however as with the cardinal plural type we'll allow a wider set of
  -- suboptimal usages that we can then lint against.
  | CardinalExactF Arg (NonEmpty (PluralCaseF PluralExact a)) a
  | CardinalInexactF Arg [PluralCaseF PluralExact a] [PluralCaseF PluralRule a] a a
  | OrdinalF Arg [PluralCaseF PluralExact a] [PluralCaseF PluralRule a] a a
  -- Plural hash references have their own distinct type rather than merely
  -- taking on `Number` to allow compilers to infer appropriately.
  | PluralRefF Arg a
  | SelectNamedF Arg (NonEmpty (SelectCaseF a)) a
  | SelectWildF Arg a a
  | SelectNamedWildF Arg (NonEmpty (SelectCaseF a)) a a
  | CallbackF Arg a a
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

type instance Base Node = NodeF

-- Concatenating two `Nodes` places the second at the tail of the first:
--   Char 'a' Fin <> Char 'b' (Char 'c' Fin) = Char 'a' (Char 'b' (Char 'c' Fin))
--
-- This is equivalent to what concatenation would look like if the sibling
-- parameter in `Node`'s constructors were removed and replaced with a list.
instance Semigroup Node where
  l <> r = case l of
    Fin                          -> r
    Char c l'                    -> Char c (l' <> r)
    Bool n t f l'                -> Bool n t f (l' <> r)
    String n l'                  -> String n (l' <> r)
    Number n l'                  -> Number n (l' <> r)
    Date n f l'                  -> Date n f (l' <> r)
    Time n f l'                  -> Time n f (l' <> r)
    CardinalExact n pe l'        -> CardinalExact n pe (l' <> r)
    CardinalInexact n pe pr w l' -> CardinalInexact n pe pr w (l' <> r)
    Ordinal n pe pr w l'         -> Ordinal n pe pr w (l' <> r)
    PluralRef n l'               -> PluralRef n (l' <> r)
    SelectNamed n c l'           -> SelectNamed n c (l' <> r)
    SelectWild n w l'            -> SelectWild n w (l' <> r)
    SelectNamedWild n c w l'     -> SelectNamedWild n c w (l' <> r)
    Callback n c l'              -> Callback n c (l' <> r)

instance Monoid Node where
  mempty = Fin

-- "abc" = Char 'a' (Char 'b' (Char 'c' Fin))
instance IsString Node where
  fromString = foldr Char Fin

-- | Consider utilising -XOverloadedStrings instead.
fromText :: Text -> Node
fromText = fromString . T.unpack

data DateTimeFmt
  = Short
  | Medium
  | Long
  | Full
  deriving (Show, Eq)

type PluralCase a = PluralCaseF a Node
type PluralCaseF a b = (a, b)

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

type SelectCase = SelectCaseF Node
type SelectCaseF a = (Text, a)

getInterpolationChildren :: Node -> Maybe Node
getInterpolationChildren = fmap snd . getInterpolation

-- Returns a tuple of an interpolation's argument name and its concatenated
-- children.
getInterpolation :: Node -> Maybe (Arg, Node)
getInterpolation Fin {}          = Nothing
getInterpolation Char {}         = Nothing
getInterpolation String {}       = Nothing
getInterpolation Number {}       = Nothing
getInterpolation Date {}         = Nothing
getInterpolation Time {}         = Nothing
getInterpolation PluralRef {}    = Nothing
getInterpolation x@(Bool {})     = Just (name x, trueCase x <> falseCase x)
getInterpolation (CardinalExact n ls _)        = Just (n, getPluralCaseNode `foldMap` ls)
getInterpolation (CardinalInexact n ls rs w _) = Just . (n,) $ mconcat
  [ getPluralCaseNode `foldMap` ls
  , getPluralCaseNode `foldMap` rs
  , w
  ]
getInterpolation (Ordinal n xs ys w _)         = Just . (n,) $ mconcat
  [ getPluralCaseNode `foldMap` xs
  , getPluralCaseNode `foldMap` ys
  , w
  ]
getInterpolation (SelectNamed n xs _)        = Just (n, snd `foldMap` xs)
getInterpolation (SelectWild n xs _)         = Just (n, xs)
getInterpolation (SelectNamedWild n xs ys _) = Just (n, snd `foldMap` xs <> ys)
getInterpolation (Callback n xs _) = Just (n, xs)

getNext :: Node -> Maybe Node
getNext Fin                         = Nothing
getNext (Char _ x)                  = Just x
getNext (String _ x)                = Just x
getNext (Number _ x)                = Just x
getNext (Date _ _ x)                = Just x
getNext (Time _ _ x)                = Just x
getNext (PluralRef _ x)             = Just x
getNext (Bool _ _ _ x)              = Just x
getNext (CardinalExact _ _ x)       = Just x
getNext (CardinalInexact _ _ _ _ x) = Just x
getNext (Ordinal _ _ _ _ x)         = Just x
getNext (SelectNamed _ _ x)         = Just x
getNext (SelectWild _ _ x)          = Just x
getNext (SelectNamedWild _ _ _ x)   = Just x
getNext (Callback _ _ x)            = Just x

getPluralCaseNode :: PluralCase a -> Node
getPluralCaseNode = snd

-- Pulls out the next node and replaces it, if any, with `Fin`.
sever :: Node -> (Node, Maybe Node)
sever = sansNext &&& getNext
  where sansNext = \case
          Fin                         -> Fin
          Char c _                    -> Char c Fin
          String n _                  -> String n Fin
          Number n _                  -> Number n Fin
          Date n f _                  -> Date n f Fin
          Time n f _                  -> Time n f Fin
          PluralRef n _               -> PluralRef n Fin
          Bool n t f _                -> Bool n t f Fin
          CardinalExact n pe _        -> CardinalExact n pe Fin
          CardinalInexact n pe pr w _ -> CardinalInexact n pe pr w Fin
          Ordinal n pe pr w _         -> Ordinal n pe pr w Fin
          SelectNamed n c _           -> SelectNamed n c Fin
          SelectWild n w _            -> SelectWild n w Fin
          SelectNamedWild n c w _     -> SelectNamedWild n c w Fin
          Callback n c _              -> Callback n c Fin

-- A series of `Node` constructor aliases which partially apply the sibling as
-- `Fin`. Particularly useful when writing out a large `Node` by hand, for
-- example in tests.
pattern Char' :: Char -> Node
pattern Char' c = Char c Fin

pattern String' :: Arg -> Node
pattern String' n = String n Fin

pattern Number' :: Arg -> Node
pattern Number' n = Number n Fin

pattern Date' :: Arg -> DateTimeFmt -> Node
pattern Date' n f = Date n f Fin

pattern Time' :: Arg -> DateTimeFmt -> Node
pattern Time' n f = Time n f Fin

pattern Bool' :: Arg -> Node -> Node -> Node
pattern Bool' n t f = Bool n t f Fin

pattern CardinalExact' :: Arg -> NonEmpty (PluralCase PluralExact) -> Node
pattern CardinalExact' n pe = CardinalExact n pe Fin

pattern CardinalInexact' :: Arg -> [PluralCase PluralExact] -> [PluralCase PluralRule] -> Node -> Node
pattern CardinalInexact' n pe pr w = CardinalInexact n pe pr w Fin

pattern Ordinal' :: Arg -> [PluralCase PluralExact] -> [PluralCase PluralRule] -> Node -> Node
pattern Ordinal' n pe pr w = Ordinal n pe pr w Fin

pattern PluralRef' :: Arg -> Node
pattern PluralRef' n = PluralRef n Fin

pattern SelectNamed' :: Arg -> NonEmpty SelectCase -> Node
pattern SelectNamed' n c = SelectNamed n c Fin

pattern SelectWild' :: Arg -> Node -> Node
pattern SelectWild' n w = SelectWild n w Fin

pattern SelectNamedWild' :: Arg -> NonEmpty SelectCase -> Node -> Node
pattern SelectNamedWild' n c w = SelectNamedWild n c w Fin

pattern Callback' :: Arg -> Node -> Node
pattern Callback' n w = Callback n w Fin
