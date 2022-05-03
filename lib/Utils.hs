module Utils where

import           Data.List (nub, (\\))
import qualified Data.Map  as M
import           Prelude

-- Borrowed from: https://hackage.haskell.org/package/intro-0.9.0.0/docs/Intro.html#v:-60--62--94-
-- | Semigroup concat lifted to an applicative context.
(<>^) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(<>^) = liftA2 (<>)
infixr 6 <>^
{-# INLINE (<>^) #-}

-- The inverse of `nub`. Output order is unspecified. Duplicates are listed
-- once in the output regardless of how often they reoccur.
dupes :: Ord a => [a] -> [a]
dupes xs = nub . sort $ xs \\ nub xs

-- Like `M.fromList`, but instead calls the given function in the presence of
-- duplicate keys. The applicative context allows for easy interop with things
-- like parsers.
toNubMap :: Applicative m => (NonEmpty Text -> m (Map Text a)) -> [(Text, a)] -> m (Map Text a)
toNubMap f xs = maybe (pure $ M.fromList xs) f . nonEmpty . dupes . fmap fst $ xs
