module Program
  ( run
  ) where

import qualified Prelude as P
import qualified Data.Char as C
import Prelude ((.), ($), (+), (-), (*))

type Pint = P.Integer

class O a where
  from :: Pint -> a
  to :: a -> Pint

data Bool a =
  False a |
  True a

data Nat =
  Zero |
  Suc Nat

data List a =
  Nil |
  Cons a (List a)

instance (O a) => O (Bool a) where
  from = u
  to (False a) = 2 * to a
  to (True a)  = 1 + 2 * to a

instance O Nat where
  from 0 = zero
  from n = suc $ from $ n - 1
  to Zero = 0
  to (Suc n) = 1 + to n

instance (O a) => O (List a) where
  from = u
  to Nil = 0
  to (Cons x xs) = u

const :: (O a, O b) => a -> b -> a
const a b = a

cast :: (O a, O b) => a -> b
cast = from . to

mu :: (O a, O b) => (a -> Bool b) -> a
mu f = mu' f 0

mu' :: (O a, O b) => (a -> Bool b) -> Pint -> a
mu' f n = let a = from n in case f a of
  False _ -> mu' f $ n + 1
  True  _ -> a

false :: (O a) => a -> Bool a
false = False

true :: (O a) => a -> Bool a
true = True

zero :: Nat
zero = Zero

suc :: Nat -> Nat
suc = Suc

nil :: (O a) => List a
nil = Nil

cons :: (O a) => a -> List a -> List a
cons = Cons

---------------------------------------------------------------------------------------------------------------------------------------

main :: List Nat -> List Nat
main a = a

---------------------------------------------------------------------------------------------------------------------------------------

run :: P.String -> P.String
run = str2pstr . main . pstr2str

pstr2str :: P.String -> List Nat
pstr2str = P.foldr (cons . char2nat) nil

str2pstr :: List Nat -> P.String
str2pstr Nil = []
str2pstr (Cons x xs) = nat2char x : str2pstr xs

char2nat :: P.Char -> Nat
char2nat = from . P.fromIntegral . C.ord

nat2char :: Nat -> P.Char
nat2char = C.chr . P.fromIntegral . to

---------------------------------------------------------------------------------------------------------------------------------------

u :: a
u = P.undefined