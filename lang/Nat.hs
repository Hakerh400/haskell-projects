module Nat
  ( Nat
  , zero
  , suc
  , nat_exa
  )
  where

import qualified Prelude as P

import Base

newtype Nat = Nat Base

zero :: Nat
zero = Nat base_leaf

suc :: Nat -> Nat
suc (Nat n) = Nat (base_node base_leaf n)

nat_exa :: (Nat -> a -> a) -> a -> Nat -> a
nat_exa f z (Nat n) = base_exa func z n where
  func _ r _ _ = f n (nat_exa f z n) where
    n = Nat r