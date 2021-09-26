module Pair
  ( Pair
  , pair
  , pair_exa
  )
  where

import qualified Prelude as P

data Pair a b = Pair a b

pair :: a -> b -> Pair a b
pair = Pair

pair_exa :: (a -> b -> c) -> Pair a b -> c
pair_exa f (Pair a b) = f a b