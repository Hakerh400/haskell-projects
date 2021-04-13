import Prelude hiding (Num)

main :: IO ()
main = putStrLn output

u :: a
u = undefined

----------------------------------------------------------------------------------------------------

data Nat =
  NatZero Zero |
  NatSucc Succ

nat_zero :: Zero -> Nat
nat_zero a = NatZero a

nat_succ :: Succ -> Nat
nat_succ a = NatSucc a

nat_extract :: (Nat -> a) -> a -> Nat -> a
nat_extract f z n = case n of
  NatSucc a -> f $ succ_extract a
  NatZero _ -> z

----------------------------------------------------------------------------------------------------

data Zero = Zero'

zero_new :: Zero
zero_new = Zero'

----------------------------------------------------------------------------------------------------

data Succ = Succ' Nat

succ_new :: Nat -> Succ
succ_new a = Succ' a

succ_extract :: Succ -> Nat
succ_extract (Succ' a) = a

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

data List a =
  ListNil (Nil a) |
  ListCons (Cons a)

list_nil :: Nil a -> List a
list_nil a = ListNil a

list_cons :: Cons a -> List a
list_cons a = ListCons a

list_extract :: (a -> List a -> b) -> b -> List a -> b
list_extract f z list = case list of
  ListCons a -> cons_extract f a
  ListNil _ -> z

----------------------------------------------------------------------------------------------------

data Nil a = Nil' a

nil_new :: Nil a
nil_new = Nil' u

----------------------------------------------------------------------------------------------------

data Cons a = Cons' a (List a)

cons_new :: a -> List a -> Cons a
cons_new a b = Cons' a b

cons_extract :: (a -> List a -> b) -> Cons a -> b
cons_extract f (Cons' a b) = f a b

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

data Num a = Num' a (Nat -> a) (a -> Nat)

num_new :: a -> (Nat -> a) -> (a -> Nat) -> Num a
num_new = Num'

num_extract :: Num a -> a
num_extract (Num' a _ _) = a

num_fromNat :: Num a -> Nat -> a
num_fromNat (Num' _ f _) = f

num_toNat :: Num a -> Nat
num_toNat (Num' a _ f) = f a

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

data Cnt = CntBase | CntSucc Cnt

cnt_base :: Cnt
cnt_base = CntBase

cnt_inc :: Cnt -> Cnt
cnt_inc = CntSucc

cnt_dec :: (Cnt -> a) -> a -> Cnt -> a
cnt_dec f z n = case n of
  (CntSucc n) -> f n
  CntBase -> z

----------------------------------------------------------------------------------------------------

nat2num :: Nat -> Num Nat
nat2num n = num_new n id id

cnt2num :: Cnt -> Num Cnt
cnt2num n = num_new n nat2cnt cnt2nat

nat2cnt :: Nat -> Cnt
nat2cnt n = nat_extract nat2cnt' cnt_base n where
  nat2cnt' :: Nat -> Cnt
  nat2cnt' n = cnt_inc $ nat2cnt n

cnt2nat :: Cnt -> Nat
cnt2nat n = cnt_dec cnt2nat' (nat_zero zero_new) n where
  cnt2nat' :: Cnt -> Nat
  cnt2nat' n = nat_succ $ succ_new $ cnt2nat n

encodeNat :: Integer -> Nat
encodeNat a = case a of
  0 -> nat_zero zero_new
  n -> nat_succ $ succ_new $ encodeNat $ n - 1

decodeNat :: Nat -> Integer
decodeNat a = nat_extract ((1+) . decodeNat) 0 a

encodeCnt :: Integer -> Num Cnt
encodeCnt n = cnt2num $ num_fromNat (cnt2num u) $ encodeNat n

decodeCnt :: Num Cnt -> Integer
decodeCnt n = decodeNat $ num_toNat n

encodeList :: [Integer] -> List (Num Cnt)
encodeList list = case list of
  [] -> list_nil nil_new
  (x:xs) -> list_cons $ cons_new (encodeCnt x) (encodeList xs)

decodeList :: List (Num Cnt) -> [Integer]
decodeList list = list_extract decodeList' [] list where
  decodeList' :: Num Cnt -> List (Num Cnt) -> [Integer]
  decodeList' x xs = decodeCnt x : decodeList xs

output :: String
output = show $ decodeList $ encodeList $ [1, 2, 3]