module Utils where

import           Prelude

-- Borrowed from: https://hackage.haskell.org/package/intro-0.9.0.0/docs/Intro.html#v:-60--62--94-
-- | Semigroup concat lifted to an applicative context.
(<>^) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(<>^) = liftA2 (<>)
infixr 6 <>^
{-# INLINE (<>^) #-}
