module Nat
  ( Nat
  , nat_zero
  , nat_suc
  , nat_exa
  )
  where

import qualified Prelude as P

data Nat = Zero
         | Suc Nat

nat_zero :: Nat
nat_zero = Zero

nat_suc :: Nat -> Nat
nat_suc = Suc

nat_exa :: (Nat -> a -> a) -> a -> Nat -> a
nat_exa f z Zero    = z
nat_exa f z (Suc n) = f n (nat_exa f z n)