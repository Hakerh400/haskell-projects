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

encode :: Integer -> Nat
encode a = case a of
  0 -> nat_zero zero_new
  n -> nat_succ $ succ_new $ encode $ n - 1

decode :: Nat -> Integer
decode a = nat_extract ((1+) . decode) 0 a

output :: String
output = show $ decode $ encode $ 123