module Utils where

import           Data.List (nubBy, (\\))
import           Prelude

-- Borrowed from: https://hackage.haskell.org/package/intro-0.9.0.0/docs/Intro.html#v:-60--62--94-
-- | Semigroup concat lifted to an applicative context.
(<>^) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(<>^) = liftA2 (<>)
infixr 6 <>^
{-# INLINE (<>^) #-}

apply2 :: a -> b -> (a -> b -> c) -> c
apply2 x y f = f x y

-- | Filters out the first instance of each found value as per the predicate.
-- The dual of `nubBy`.
bunBy :: (Foldable f, Eq a) => (a -> a -> Bool) -> f a -> [a]
bunBy f xs = let ys = toList xs in ys \\ nubBy f ys

-- | Filters out the first instance of each found value as per its `Eq`
-- instance. The dual of `nub`.
bun :: (Foldable f, Eq a) => f a -> [a]
bun = bunBy (==)
