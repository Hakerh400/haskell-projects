main :: IO ()
main = print output

output :: Integer
output = nat2int $ fix (`mul` Z)

data N = Z | S N

int2nat :: Integer -> N
int2nat 0 = Z
int2nat n = S (int2nat (n - 1))

nat2int :: N -> Integer
nat2int Z = 0
nat2int (S a) = 1 + nat2int a

add :: N -> N -> N
add a Z = a
add a (S b) = S (add a b)

mul :: N -> N -> N
mul a Z = Z
mul a (S b) = add a (mul a b)

fix :: (a -> a) -> a
fix f = f (fix f)

u :: a
u = undefined