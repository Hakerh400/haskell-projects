import System

main :: IO ()
main = putStrLn output

ite :: Bool -> a -> a -> a
ite a b c = if a then b else c;

u :: a
u = undefined

----------------------------------------------------------------------------------------------------

nat a = f "nat" [a]
inc a = f "succ" [nat a]
ident a = f "ident" [nat a]

zero = f "zero" []
one = inc zero
two = inc one

a = ident zero

proof = f "wff" [f "all" [f "abs" [a, f "wff" [a]]]]

----------------------------------------------------------------------------------------------------

output :: String
output = show proof