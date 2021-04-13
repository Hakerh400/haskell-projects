{-# LANGUAGE FlexibleInstances #-}

module Core (
  int2nat,
  nat2int,
  (<=),
  Nat,
  Mu(..)
) where

import qualified Prelude as P

data Nat =
  Zero |
  Succ Nat

for :: (P.Eq a, P.Num a) => (b -> b) -> b -> a -> b
for f z 0 = z
for f z n = f (for f z ((P.-) n 1))

int2nat :: P.Integer -> Nat
int2nat = for Succ Zero

nat2int :: Nat -> P.Integer
nat2int Zero = 0
nat2int (Succ a) = (P.+) 1 (nat2int a)

zero :: Nat
zero = Zero

one :: Nat
one = Succ zero

(<=) :: Nat -> Nat -> Nat
Zero <= _ = one
(Succ a) <= (Succ b) = a <= b
_ <= _ = zero

class Mu a where
  mu :: (Nat -> a) -> a

instance Mu Nat where
  mu f = mu' f zero

instance Mu (Nat -> Nat) where
  mu f a = mu' (f a) zero

instance Mu (Nat -> Nat -> Nat) where
  mu f a b = mu' (f a b) zero
   
mu' :: (Nat -> Nat) -> Nat -> Nat
mu' f n = case f n of
  Zero -> mu' f (Succ n)
  _ -> n