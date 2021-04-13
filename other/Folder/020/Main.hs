{-# LANGUAGE ScopedTypeVariables #-}

import System

main :: IO ()
main = putStrLn output

ite :: Bool -> a -> a -> a
ite a b c = if a then b else c;

u :: a
u = undefined

----------------------------------------------------------------------------------------------------

inc a = f "succ" [f "nat" [a]]

zero = f "zero" []
one = inc zero
two = inc one

proof = f "nat" [two]

----------------------------------------------------------------------------------------------------

output :: String
output = show proof