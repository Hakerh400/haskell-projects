{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}

import Prelude hiding (Monad, return, (>>=))

import qualified Data.Set as Set

class Monad m a where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Set a where
  Empty :: Set a
  Wrap :: Ord a => Set.Set a -> Set a

instance (Ord a, Show a) => Show (Set a) where
  show = show . toLegacySet

instance Ord a => Monad Set a where
  return = singleton
  s >>= f = fold (union . f) Empty s

toLegacySet :: (Ord a) => Set a -> Set.Set a
toLegacySet Empty = Set.empty
toLegacySet (Wrap s) = s

singleton :: Ord a => a -> Set a
singleton = Wrap . Set.singleton

fromList :: Ord a => [a] -> Set a
fromList = Wrap . Set.fromList

union :: Set a -> Set a -> Set a
union Empty t = t
union s Empty = s
union (Wrap s) (Wrap t) = Wrap (Set.union s t)

fold :: (a -> b -> b) -> b -> Set a -> b
fold _ z Empty = z
fold f z (Wrap s) = Set.fold f z s

main :: IO ()
main = print set

set :: Set Int
set = do
  a <- fromList [1, 2, 3]
  b <- fromList [1, 2, 3]
  return $ a + b