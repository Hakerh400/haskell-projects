module Program
  ( main
  ) where

import qualified Prelude as P

import Minimization
import Function
import Bool
import Pair
import Maybe
import Nat
import List

--
-- Function
--

-- ###
const :: a -> b -> a
const = comb_k

const2 :: a -> b -> c -> a
const2 = const . const

id :: a -> a
id a = a

infixr 9 .
(.) :: (a -> b) -> (c -> a) -> c -> b
(.) f g x = f (g x)

dot2 :: (a -> b) -> (c -> d -> a) -> c -> d -> b
dot2 f g x y = f (g x y)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- ###
fst_arg :: a -> b -> a
fst_arg = const

snd_arg :: a -> b -> b
snd_arg = const id

--
-- Bool
--

-- ###
ite :: Bool -> a -> a -> a
ite b x y = bool_exa x y b

--
-- Pair
--

-- ###
uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry = pair_exa

fst :: Pair a b -> a
fst = uncurry fst_arg

snd :: Pair a b -> b
snd = uncurry snd_arg

--
-- Nat
--

instance P.Num Nat where
  (+) = nat_add
  (-) = nat_sub
  (*) = nat_mul
  abs = id
  signum = nat_signum
  fromInteger 0 = zero
  fromInteger n = suc (P.fromInteger ((P.-) n 1))

nat_case :: (Nat -> a) -> a -> Nat -> a
nat_case f = nat_exa (const . f)

nat_fold :: (a -> a) -> a -> Nat -> a
nat_fold = nat_exa . const

-- ###
nat_inc :: Nat -> Nat
nat_inc = suc

nat_dec :: Nat -> Nat
nat_dec = nat_exa const zero

nat_add :: Nat -> Nat -> Nat
nat_add = nat_fold nat_inc

nat_sub :: Nat -> Nat -> Nat
nat_sub = nat_fold nat_dec

nat_mul :: Nat -> Nat -> Nat
nat_mul a = nat_fold (nat_add a) zero

nat_signum :: Nat -> Nat
nat_signum = nat_case (const 1) 0

--
-- List
--

infixr 5 ++
(++) :: List a -> List a -> List a
(++) xs ys = foldr cons ys xs

head :: a -> List a -> a
head = list_case fst_arg

last :: a -> List a -> a
last z = head z . reverse

tail :: List a -> List a
tail = list_case snd_arg nil

init :: List a -> List a
init = reversed tail

uncons :: List a -> Maybe (Pair a (List a))
uncons = list_case (dot2 just pair) nothing

null :: List a -> Bool
null = list_case (const2 true) false

length :: List a -> Nat
length = foldr (const suc) 0

map :: (a -> b) -> List a -> List b
map f = foldr (cons . f) nil

reverse :: List a -> List a
reverse = foldl (flip cons) nil

intersperse :: a -> List a -> List a
intersperse e = tail . foldr ((.) (cons e) . cons) nil

intercalate :: List a -> List (List a) -> List a
intercalate = dot2 concat intersperse

concat :: List (List a) -> List a
concat = foldr (++) nil

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f = list_exa (const . f)

foldl :: (b -> a -> b) -> b -> List a -> b
foldl f z xs = foldr foldl' id xs z where
  foldl' x g z = g (f z x)

singleton :: a -> List a
singleton a = cons a nil

filter :: (a -> Bool) -> List a -> List a
filter f = foldr filter' nil where
  filter' x = ite (f x) (cons x) id

-- break :: (a -> Bool) -> List a -> Pair (List a) (List a)
-- break f = foldl break' where
--   break' xs x = ite (f x)

list_case :: (a -> List a -> b) -> b -> List a -> b
list_case f = list_exa (dot2 const f)

reversed :: (List a -> List b) -> List a -> List b
reversed f = reverse . f . reverse

--
-- Main
--

main :: List Nat -> List Nat
main a = intercalate (cons 46 (cons 46 nil)) (map singleton a)