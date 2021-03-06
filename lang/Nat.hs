module Nat
  ( Nat
  , zero
  , suc
  , nat_exa
  , nat_lit
  ) where

import qualified Prelude as P

data Nat
  = Zero
  | Suc Nat

zero :: Nat
zero = Zero

suc :: Nat -> Nat
suc = Suc

nat_exa :: (Nat -> a -> a) -> a -> Nat -> a
nat_exa f z Zero = z
nat_exa f z (Suc n) = f n (nat_exa f z n)

nat_lit :: (P.Num a, P.Eq a) => a -> Nat
nat_lit 0 = Nat.zero
nat_lit n = Nat.suc (nat_lit ((P.-) n 1))