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

encodeNat :: Integer -> Nat
encodeNat a = case a of
  0 -> nat_zero zero_new
  n -> nat_succ $ succ_new $ encodeNat $ n - 1

decodeNat :: Nat -> Integer
decodeNat a = nat_extract ((1+) . decodeNat) 0 a

encodeList :: [Integer] -> List Nat
encodeList list = case list of
  [] -> list_nil nil_new
  (x:xs) -> list_cons $ cons_new (encodeNat x) (encodeList xs)

decodeList :: List Nat -> [Integer]
decodeList list = list_extract decodeList' [] list where
  decodeList' :: Nat -> List Nat -> [Integer]
  decodeList' x xs = decodeNat x : decodeList xs

output :: String
output = show $ decodeList $ encodeList $ [1, 2, 3]