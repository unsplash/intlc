-- This module defines an AST for ICU messages. We do not necessarily behave
-- identically to other implementations.
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

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
  deriving newtype (Show, Eq, Ord, IsString)

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
    FinF                          -> r
    CharF c l'                    -> CharF c (l' `fconcat` r)
    BoolF n t f l'                -> BoolF n t f (l' `fconcat` r)
    StringF n l'                  -> StringF n (l' `fconcat` r)
    NumberF n l'                  -> NumberF n (l' `fconcat` r)
    DateF n f l'                  -> DateF n f (l' `fconcat` r)
    TimeF n f l'                  -> TimeF n f (l' `fconcat` r)
    CardinalExactF n pe l'        -> CardinalExactF n pe (l' `fconcat` r)
    CardinalInexactF n pe pr w l' -> CardinalInexactF n pe pr w (l' `fconcat` r)
    OrdinalF n pe pr w l'         -> OrdinalF n pe pr w (l' `fconcat` r)
    PluralRefF n l'               -> PluralRefF n (l' `fconcat` r)
    SelectNamedF n c l'           -> SelectNamedF n c (l' `fconcat` r)
    SelectWildF n w l'            -> SelectWildF n w (l' `fconcat` r)
    SelectNamedWildF n c w l'     -> SelectNamedWildF n c w (l' `fconcat` r)
    CallbackF n c l'              -> CallbackF n c (l' `fconcat` r)
    where fconcat x y = embed $ project x <> y

instance Semigroup Node where
  l <> r = embed (project l <> project r)

instance Monoid (NodeF Node) where
  mempty = FinF

instance Monoid Node where
  mempty = embed FinF

-- "abc" = Char 'a' (Char 'b' (Char 'c' Fin))
instance IsString Node where
  fromString = foldr (\c x -> embed (CharF c x)) (embed FinF)

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
  deriving newtype (Show, Eq, IsString)

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

sansAnnMsg :: Message AnnNode -> Message Node
sansAnnMsg = fmap sansAnn

getNext :: NodeF a -> Maybe a
getNext FinF                         = Nothing
getNext (CharF _ x)                  = Just x
getNext (StringF _ x)                = Just x
getNext (NumberF _ x)                = Just x
getNext (DateF _ _ x)                = Just x
getNext (TimeF _ _ x)                = Just x
getNext (PluralRefF _ x)             = Just x
getNext (BoolF _ _ _ x)              = Just x
getNext (CardinalExactF _ _ x)       = Just x
getNext (CardinalInexactF _ _ _ _ x) = Just x
getNext (OrdinalF _ _ _ _ x)         = Just x
getNext (SelectNamedF _ _ x)         = Just x
getNext (SelectWildF _ _ x)          = Just x
getNext (SelectNamedWildF _ _ _ x)   = Just x
getNext (CallbackF _ _ x)            = Just x

-- Pulls out the next node and replaces it, if any, with `Fin`.
sever :: Node -> (Node, Maybe Node)
sever = (sansNext &&& getNext) . project
  where sansNext = \case
          FinF                         -> embed FinF
          CharF c _                    -> Char' c
          StringF n _                  -> String' n
          NumberF n _                  -> Number' n
          DateF n f _                  -> Date' n f
          TimeF n f _                  -> Time' n f
          PluralRefF n _               -> PluralRef' n
          BoolF n t f _                -> Bool' n t f
          CardinalExactF n pe _        -> CardinalExact' n pe
          CardinalInexactF n pe pr w _ -> CardinalInexact' n pe pr w
          OrdinalF n pe pr w _         -> Ordinal' n pe pr w
          SelectNamedF n c _           -> SelectNamed' n c
          SelectWildF n w _            -> SelectWild' n w
          SelectNamedWildF n c w _     -> SelectNamedWild' n c w
          CallbackF n c _              -> Callback' n c

-- A series of `Node` constructor aliases which partially apply the sibling as
-- `Fin`. Particularly useful when writing out a large `Node` by hand, for
-- example in tests.
--
-- It looks like pattern constructors can't make use of some abstractions, hence
-- the direct use of the `Fix` constructor.
pattern Char' :: Char -> Node
pattern Char' c = Fix (CharF c (Fix FinF))

pattern String' :: Arg -> Node
pattern String' n = Fix (StringF n (Fix FinF))

pattern Number' :: Arg -> Node
pattern Number' n = Fix (NumberF n (Fix FinF))

pattern Date' :: Arg -> DateTimeFmt -> Node
pattern Date' n f = Fix (DateF n f (Fix FinF))

pattern Time' :: Arg -> DateTimeFmt -> Node
pattern Time' n f = Fix (TimeF n f (Fix FinF))

pattern Bool' :: Arg -> Node -> Node -> Node
pattern Bool' n t f = Fix (BoolF n t f (Fix FinF))

pattern CardinalExact' :: Arg -> NonEmpty (PluralCase PluralExact) -> Node
pattern CardinalExact' n pe = Fix (CardinalExactF n pe (Fix FinF))

pattern CardinalInexact' :: Arg -> [PluralCase PluralExact] -> [PluralCase PluralRule] -> Node -> Node
pattern CardinalInexact' n pe pr w = Fix (CardinalInexactF n pe pr w (Fix FinF))

pattern Ordinal' :: Arg -> [PluralCase PluralExact] -> [PluralCase PluralRule] -> Node -> Node
pattern Ordinal' n pe pr w = Fix (OrdinalF n pe pr w (Fix FinF))

pattern PluralRef' :: Arg -> Node
pattern PluralRef' n = Fix (PluralRefF n (Fix FinF))

pattern SelectNamed' :: Arg -> NonEmpty SelectCase -> Node
pattern SelectNamed' n c = Fix (SelectNamedF n c (Fix FinF))

pattern SelectWild' :: Arg -> Node -> Node
pattern SelectWild' n w = Fix (SelectWildF n w (Fix FinF))

pattern SelectNamedWild' :: Arg -> NonEmpty SelectCase -> Node -> Node
pattern SelectNamedWild' n c w = Fix (SelectNamedWildF n c w (Fix FinF))

pattern Callback' :: Arg -> Node -> Node
pattern Callback' n w = Fix (CallbackF n w (Fix FinF))
