{-# LANGUAGE NoImplicitPrelude, GADTs, ScopedTypeVariables #-}

module CustomList (
  CustomList(..)
) where

import Prelude (Show, String, ($), show)
import Data.List (intercalate)

id :: a -> a
id a = a

class CustomList a where
  empty :: a b
  cons :: b -> a b -> a b
  pat :: a b -> c -> (b -> a b -> c) -> c

  map :: forall b c d. (CustomList d) => a b -> (b -> c) -> d c
  map x f = pat x empty next where
    next :: b -> a b -> d c
    next y z = cons (f y) (map z f)

  convert :: (CustomList c) => a b -> c b
  convert x = map x id

  toStr :: (Show b) => a b -> String
  toStr x = intercalate [] $ map x show

instance CustomList [] where
  empty = []
  cons = (:)
  pat a b c = case a of
    [] -> b
    (x:xs) -> c x xs