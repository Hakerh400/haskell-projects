import System

main :: IO ()
main = putStrLn output

ite :: Bool -> a -> a -> a
ite a b c = if a then b else c;

u :: a
u = undefined

----------------------------------------------------------------------------------------------------

nat a = f F_nat [a]
inc a = f F_succ [nat a]
ident a = f F_ident [nat a]

zero = f F_zero []
one = inc zero
two = inc one

a = ident zero

proof = f F_wff [f F_all [f F_abs [a, f F_wff [a]]]]

----------------------------------------------------------------------------------------------------

output :: String
output = show proof