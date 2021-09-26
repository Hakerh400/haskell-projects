module Function
  ( comb_k
  , comb_s
  ) where

import qualified Prelude as P

comb_k :: a -> b -> a
comb_k a b = a

comb_s :: (a -> b -> c) -> (a -> b) -> a -> c
comb_s a b c = a c (b c)