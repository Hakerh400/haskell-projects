module Minimization
  ( minimize
  )
  where

import qualified Prelude as P

import Bool
import Nat

minimize :: (Nat -> Bool) -> Nat
minimize f = minimize' f nat_zero

minimize' :: (Nat -> Bool) -> Nat -> Nat
minimize' f n = bool_exa
  n
  (minimize' f (nat_suc n))
  (f n)