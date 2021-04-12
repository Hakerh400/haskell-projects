-- import System

main :: IO ()
main = putStrLn output

ite :: Bool -> a -> a -> a
ite a b c = if a then b else c

u :: a
u = undefined

----------------------------------------------------------------------------------------------------

-- nat a = f F_nat [a]
-- inc a = f F_succ [nat a]
-- ident a = f F_ident [nat a]

-- zero = f F_zero []
-- one = inc zero
-- two = inc one

-- a = ident zero

-- proof = f F_wff [f F_all [f F_abs [a, f F_wff [a]]]]

----------------------------------------------------------------------------------------------------

output :: String
output = show result

result = func1 [1..100]

func1 [] = 0
func1 (a:b) = let m = func1 b in a + ((m + m) `div` 2)

func2 [] = 0
func2 (a:b) = a + ((func2 b + func2 b) `div` 2)