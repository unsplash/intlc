-- This module defines an AST for ICU messages. We do not necessarily behave
-- identically to other implementations.
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TemplateHaskell  #-}

module Intlc.ICU where

import           Control.Comonad.Cofree       (Cofree)
import           Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import           Data.Eq.Deriving             (deriveEq1)
import           Data.Fix                     (Fix (Fix))
import           Data.Functor.Foldable        (cata, embed, project)
import           Prelude
import           Text.Show.Deriving           (deriveShow1)

newtype Message a = Message a
  deriving (Show, Eq, Functor)

unMessage :: Message a -> a
unMessage (Message x) = x

newtype Arg = Arg Text
  deriving (Show, Eq, Ord, IsString)

unArg :: Arg -> Text
unArg (Arg x) = x

-- | A `NodeF` is either an interpolation - some sort of identifier for input -
-- or mere plaintext. A collection of nodes make up any message. The entire AST
-- is represented as a single recursive node with the trailing `NodeF` always
-- representing the following sibling. Termination is represented by `Fin`,
-- equivalent to a list's `Nil`.
--
-- On interpolations we diverge from icu4j by supporting a boolean type, and
-- not necessarily requiring wildcard cases.
--
-- This core type is represented as a "pattern functor". Useful for recursion
-- schemes and pairing additional data to nodes via the likes of `Cofree`.
data NodeF a
  = Fin
  | Char
    { char :: Char
    , next :: a
    }
  | Bool
    { arg       :: Arg
    , trueCase  :: a
    , falseCase :: a
    , next      :: a
    }
  | String
    { arg  :: Arg
    , next :: a
    }
  | Number
    { arg  :: Arg
    , next :: a
    }
  | Date
    { arg    :: Arg
    , format :: DateTimeFmt
    , next   :: a
    }
  | Time
    { arg    :: Arg
    , format :: DateTimeFmt
    , next   :: a
    }
  -- The only cardinal plurals which do not require a wildcard are those
  -- consisting solely of literal/exact cases. This is because within the AST we
  -- only care about correctness and prospective type safety, not optimal use of
  -- ICU syntax.
  | CardinalExact
    { arg          :: Arg
    , exactCasesNE :: NonEmpty (PluralCaseF PluralExact a)
    , next         :: a
    }
  | CardinalInexact
    { arg        :: Arg
    , exactCases :: [PluralCaseF PluralExact a]
    , ruleCases  :: [PluralCaseF PluralRule a]
    , wildcard   :: a
    , next       :: a
    }
  -- Ordinal plurals always require a wildcard as per their intended usage with
  -- rules, however as with the cardinal plural type we'll allow a wider set of
  -- suboptimal usages that we can then lint against.
  | Ordinal
    { arg        :: Arg
    , exactCases :: [PluralCaseF PluralExact a]
    , ruleCases  :: [PluralCaseF PluralRule a]
    , wildcard   :: a
    , next       :: a
    }
  -- Plural hash references have their own distinct type rather than merely
  -- taking on `Number` to allow compilers to infer appropriately.
  | PluralRef
    { arg  :: Arg
    , next :: a
    }
  | SelectNamed
    { arg         :: Arg
    , selectCases :: NonEmpty (SelectCaseF a)
    , next        :: a
    }
  | SelectWild
    { arg      :: Arg
    , wildcard :: a
    , next     :: a
    }
  | SelectNamedWild
    { arg         :: Arg
    , selectCases :: NonEmpty (SelectCaseF a)
    , wildcard    :: a
    , next        :: a
    }
  | Callback
    { arg   :: Arg
    , child :: a
    , next  :: a
    }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

-- | `NodeF` recursing on itself, forming a typical, simple AST. By convention
-- `Fix` won't generally be referenced directly, with code instead leaning on
-- recursion schemes' `project` and `embed`, which themselves are implemented
-- for `Fix`.
type Node = Fix NodeF

-- | A `Node` annotated with an `Int` representing a source offset.
type AnnNode = Cofree NodeF Int

-- | Drop all annotations from an AST/`Node`.
sansAnn :: AnnNode -> Node
-- Explanation: https://stackoverflow.com/a/51050171/3369753
sansAnn = cata $ \(_ :< x) -> embed x

-- Concatenating two `NodeF Node`s places the second at the tail of the first:
--   Char 'a' Fin <> Char 'b' (Char 'c' Fin) = Char 'a' (Char 'b' (Char 'c' Fin))
--
-- This is equivalent to what concatenation would look like if the sibling
-- parameter in `Node`'s constructors were removed and replaced with a list.
instance Semigroup (NodeF Node) where
  l <> r = case l of
    Fin                          -> r
    Char c l'                    -> Char c (l' `fconcat` r)
    Bool n t f l'                -> Bool n t f (l' `fconcat` r)
    String n l'                  -> String n (l' `fconcat` r)
    Number n l'                  -> Number n (l' `fconcat` r)
    Date n f l'                  -> Date n f (l' `fconcat` r)
    Time n f l'                  -> Time n f (l' `fconcat` r)
    CardinalExact n pe l'        -> CardinalExact n pe (l' `fconcat` r)
    CardinalInexact n pe pr w l' -> CardinalInexact n pe pr w (l' `fconcat` r)
    Ordinal n pe pr w l'         -> Ordinal n pe pr w (l' `fconcat` r)
    PluralRef n l'               -> PluralRef n (l' `fconcat` r)
    SelectNamed n c l'           -> SelectNamed n c (l' `fconcat` r)
    SelectWild n w l'            -> SelectWild n w (l' `fconcat` r)
    SelectNamedWild n c w l'     -> SelectNamedWild n c w (l' `fconcat` r)
    Callback n c l'              -> Callback n c (l' `fconcat` r)
    where fconcat x y = embed $ project x <> y

instance Semigroup Node where
  l <> r = embed (project l <> project r)

instance Monoid (NodeF Node) where
  mempty = Fin

instance Monoid Node where
  mempty = embed Fin

-- "abc" = Char 'a' (Char 'b' (Char 'c' Fin))
instance IsString Node where
  fromString = foldr (\c x -> embed (Char c x)) (embed Fin)

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
  deriving (Show, Eq, IsString)

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

-- Use Template Haskell to generate lifted typeclass instances for `NodeF`.
-- Needs to appear after all the type aliases that `NodeF` references are
-- defined. Anything else leaning on these instances must appear after this
-- point.
$(deriveShow1 ''NodeF)
$(deriveEq1   ''NodeF)

getNext :: NodeF a -> Maybe a
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

-- Pulls out the next node and replaces it, if any, with `Fin`.
sever :: Node -> (Node, Maybe Node)
sever = (sansNext &&& getNext) . project
  where sansNext = \case
          Fin                         -> embed Fin
          Char c _                    -> Char' c
          String n _                  -> String' n
          Number n _                  -> Number' n
          Date n f _                  -> Date' n f
          Time n f _                  -> Time' n f
          PluralRef n _               -> PluralRef' n
          Bool n t f _                -> Bool' n t f
          CardinalExact n pe _        -> CardinalExact' n pe
          CardinalInexact n pe pr w _ -> CardinalInexact' n pe pr w
          Ordinal n pe pr w _         -> Ordinal' n pe pr w
          SelectNamed n c _           -> SelectNamed' n c
          SelectWild n w _            -> SelectWild' n w
          SelectNamedWild n c w _     -> SelectNamedWild' n c w
          Callback n c _              -> Callback' n c

-- A series of `Node` constructor aliases which partially apply the sibling as
-- `Fin`. Particularly useful when writing out a large `Node` by hand, for
-- example in tests.
--
-- It looks like pattern constructors can't make use of some abstractions, hence
-- the direct use of the `Fix` constructor.
pattern Char' :: Char -> Node
pattern Char' c = Fix (Char c (Fix Fin))

pattern String' :: Arg -> Node
pattern String' n = Fix (String n (Fix Fin))

pattern Number' :: Arg -> Node
pattern Number' n = Fix (Number n (Fix Fin))

pattern Date' :: Arg -> DateTimeFmt -> Node
pattern Date' n f = Fix (Date n f (Fix Fin))

pattern Time' :: Arg -> DateTimeFmt -> Node
pattern Time' n f = Fix (Time n f (Fix Fin))

pattern Bool' :: Arg -> Node -> Node -> Node
pattern Bool' n t f = Fix (Bool n t f (Fix Fin))

pattern CardinalExact' :: Arg -> NonEmpty (PluralCase PluralExact) -> Node
pattern CardinalExact' n pe = Fix (CardinalExact n pe (Fix Fin))

pattern CardinalInexact' :: Arg -> [PluralCase PluralExact] -> [PluralCase PluralRule] -> Node -> Node
pattern CardinalInexact' n pe pr w = Fix (CardinalInexact n pe pr w (Fix Fin))

pattern Ordinal' :: Arg -> [PluralCase PluralExact] -> [PluralCase PluralRule] -> Node -> Node
pattern Ordinal' n pe pr w = Fix (Ordinal n pe pr w (Fix Fin))

pattern PluralRef' :: Arg -> Node
pattern PluralRef' n = Fix (PluralRef n (Fix Fin))

pattern SelectNamed' :: Arg -> NonEmpty SelectCase -> Node
pattern SelectNamed' n c = Fix (SelectNamed n c (Fix Fin))

pattern SelectWild' :: Arg -> Node -> Node
pattern SelectWild' n w = Fix (SelectWild n w (Fix Fin))

pattern SelectNamedWild' :: Arg -> NonEmpty SelectCase -> Node -> Node
pattern SelectNamedWild' n c w = Fix (SelectNamedWild n c w (Fix Fin))

pattern Callback' :: Arg -> Node -> Node
pattern Callback' n w = Fix (Callback n w (Fix Fin))
