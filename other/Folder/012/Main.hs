{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

import Prelude hiding (succ)

main :: IO ()
main = putStrLn output

u :: a
u = undefined

----------------------------------------------------------------------------------------------------

data A = A String [A]

----------------------------------------------------------------------------------------------------

zero = A "zero" []

succ a = case a of
  A "nat" (_:[]) -> A "succ" [a]

nat a = case a of
  A "zero" [] -> A "nat" [a]
  A "succ" (_:[]) -> A "nat" [a]

----------------------------------------------------------------------------------------------------

int2nat :: Integer -> A
int2nat 0 = nat $ zero
int2nat a = nat $ succ $ int2nat $ a - 1

nat2int :: A -> Integer
nat2int (A "nat" (a:[])) = case a of
  A "zero" [] -> 0
  A "succ" (b:[]) -> 1 + (nat2int b)

output :: String
output = show $ nat2int $ int2nat $ 123