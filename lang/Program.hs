module Program
  ( main
  ) where

import qualified Prelude as P

import Minimization
import Function
import Bool
import Nat
import List

--
-- Function
--

-- ###
const :: a -> b -> a
const = comb_k

id :: a -> a
id a = a

infixr 9 .
(.) :: (a -> b) -> (c -> a) -> c -> b
(.) f g x = f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

--
-- Bool
--

-- ###
ite :: Bool -> a -> a -> a
ite b x y = bool_exa x y b

--
-- Nat
--

instance P.Num Nat where
  (+) = nat_add
  (-) = nat_sub
  (*) = nat_mul
  abs = id
  signum = nat_signum
  fromInteger 0 = nat_zero
  fromInteger n = nat_suc (P.fromInteger ((P.-) n 1))

nat_case :: (Nat -> a) -> a -> Nat -> a
nat_case f = nat_exa (const . f)

nat_fold :: (a -> a) -> a -> Nat -> a
nat_fold = nat_exa . const

-- ###
nat_inc :: Nat -> Nat
nat_inc = nat_suc

nat_dec :: Nat -> Nat
nat_dec = nat_exa const nat_zero

nat_add :: Nat -> Nat -> Nat
nat_add = nat_fold nat_inc

nat_sub :: Nat -> Nat -> Nat
nat_sub = nat_fold nat_dec

nat_mul :: Nat -> Nat -> Nat
nat_mul a = nat_fold (nat_add a) nat_zero

nat_signum :: Nat -> Nat
nat_signum = nat_case (const 1) 0

--
-- List
--

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f = list_exa (const . f)

foldl :: (b -> a -> b) -> b -> List a -> b
foldl f z xs = foldr foldl' id xs z where
  foldl' x g z = g (f z x)

map :: (a -> b) -> List a -> List b
map f = foldr (list_cons . f) list_nil

reverse :: List a -> List a
reverse = foldl (flip list_cons) list_nil

append :: List a -> List a -> List a
append xs ys = foldr list_cons ys xs

singleton :: a -> List a
singleton a = list_cons a list_nil

filter :: (a -> Bool) -> List a -> List a
filter f = foldr filter' list_nil where
  filter' x xs = ite (f x) (list_cons x xs) xs

--
-- Main
--

main :: List Nat -> List Nat
main a = a