{-# LANGUAGE RankNTypes #-}

newtype N = N {cn :: forall a. (a -> a) -> a -> a}

main :: IO ()
main = print $ cn2int result

result :: N
result = ex (int2cn 2) (int2cn 10)

int2cn :: Int -> N
int2cn 0 = cZero
int2cn n = inc $ int2cn $ n - 1

cn2int :: N -> Int
cn2int n = cn n (+1) 0

cZero :: N
cZero = N $ const id

cOne :: N
cOne = inc cZero

ite :: N -> a -> a -> a
ite (N n) x y = n (const x) y

pair :: N -> N -> N -> N
pair x y n = ite n y x

fs :: (N -> N) -> N
fs p = p cZero

sn :: (N -> N) -> N
sn p = p cOne

inc :: N -> N
inc (N n) = N $ \f x -> f $ n f x

dec :: N -> N
dec (N n) = sn $ n dec' (pair cZero u)

dec' :: (N -> N) -> N -> N
dec' p = let
  a = fs p
  b = sn p
  in pair (inc a) (ite a a cZero)

add :: N -> N -> N
add x (N n) = n inc x

sub :: N -> N -> N
sub x (N n) = n dec x

mul :: N -> N -> N
mul x (N n) = n (add x) cZero

ex :: N -> N -> N
ex (N m) (N n) = N $ n m

u :: a
u = undefined