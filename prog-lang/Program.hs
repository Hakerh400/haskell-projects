module Program where

type N = Integer

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
from_pair (Pair a b) = u

to_pair :: N -> Pair
to_pair n = u

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

u :: a
u = undefined