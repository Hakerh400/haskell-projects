module Program where

import Control.Monad
import Prelude hiding (read)
import qualified Serializer as S

type N = Integer
type Prog = N -> N

class Data t where
  from_hs :: t -> N
  to_hs :: N -> t

instance (Data t1, Data t2) => Data (t1, t2) where
  from_hs (a, b) = pair (from_hs a) (from_hs b)
  to_hs = pair_get $ \a b -> (to_hs a, to_hs b)

instance Data Integer where
  from_hs = id
  to_hs = id

instance (Data t) => Data [t] where
  from_hs = foldr (cons . from_hs) nil
  to_hs = list_fold [] $ (:) . to_hs

run :: (Data t1, Data t2) => Prog -> t1 -> t2
run prog = to_hs . prog . from_hs

data Unit = Unit N

from_unit :: Unit -> N
from_unit (Unit n) = n

to_unit :: N -> Unit
to_unit = Unit

unit :: N -> N
unit = from_unit . Unit

unit_get :: (N -> a) -> N -> a
unit_get f u = case to_unit u of
  Unit n -> f n

unit_fold :: (N -> a) -> N -> a
unit_fold = unit_get

data Pair = Pair N N

from_pair :: Pair -> N
from_pair (Pair a b) = S.ser $ do
  S.write_nat a
  S.write_nat' b

to_pair :: N -> Pair
to_pair = S.deser $ do
  a <- S.read_nat
  b <- S.read_nat'
  return $ Pair a b

pair :: N -> N -> N
pair a b = from_pair $ Pair a b

pair_get :: (N -> N -> a) -> N -> a
pair_get f p = case to_pair p of
  Pair a b -> f a b

pair_fold :: (N -> N -> a) -> N -> a
pair_fold = pair_get

data Bool' =
  False' N |
  True'  N

from_bool :: Bool' -> N
from_bool (False' n) = n * 2
from_bool (True'  n) = n * 2 + 1

to_bool :: N -> Bool'
to_bool n = let
  ctor = if odd n then True' else False'
  in ctor $ n `div` 2

false :: N -> N
false = from_bool . False'

true :: N -> N
true = from_bool . True'

bool_get :: (N -> a) -> (N -> a) -> N -> a
bool_get f t b = case to_bool b of
  False' n -> f n
  True'  n -> t n

bool_fold :: (N -> a) -> (N -> a) -> N -> a
bool_fold = bool_get

mu :: (N -> N) -> N
mu f = mu' f 0 where
  mu' f n = nat_get (mu' f $ suc n) id (f n)

data Nat =
  Zero |
  Suc Nat

from_nat :: Nat -> N
from_nat Zero = 0
from_nat (Suc n) = 1 + from_nat n

to_nat :: N -> Nat
to_nat 0 = Zero
to_nat n = Suc $ to_nat $ n - 1

zero :: N
zero = from_nat Zero

suc :: N -> N
suc = from_nat . Suc . to_nat

nat_get :: a -> (N -> a) -> N -> a
nat_get z f n = case to_nat n of
  Zero  -> z
  Suc n -> f $ from_nat n

nat_fold :: a -> (a -> a) -> N -> a
nat_fold z f n = nat_fold' z f $ to_nat n where
  nat_fold' z _  Zero   = z
  nat_fold' z f (Suc n) = f $ nat_fold' z f n

data List =
  Nil |
  Cons N List

from_list :: List -> N
from_list = S.ser . from_list' where
  from_list' Nil = return ()
  from_list' (Cons x xs) = do
    S.inc1
    S.write_nat x
    from_list' xs

to_list :: N -> List
to_list = S.deser to_list' where
  to_list' = do
    b <- S.nz
    if b
      then do
        x <- S.read_nat
        xs <- to_list'
        return $ Cons x xs
      else return Nil

nil :: N
nil = from_list Nil

cons :: N -> N -> N
cons x xs = from_list $ Cons x $ to_list xs

list_get :: a -> (N -> N -> a) -> N -> a
list_get z f n = case to_list n of
  Nil -> z
  Cons x xs -> f x $ from_list xs

list_fold :: a -> (N -> a -> a) -> N -> a
list_fold z f n = case to_list n of
  Nil -> z
  Cons x xs -> f x $ list_fold z f $ from_list xs

u :: a
u = undefined