{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = putStrLn output

ite :: Bool -> a -> a -> a
ite a b c = if a then b else c;

u :: a
u = undefined

----------------------------------------------------------------------------------------------------

class Minimizable a where
  minimize :: (a -> Bool) -> a

data Nat = Zero | Succ Nat

instance Minimizable Nat where
  minimize f = minimize' f Zero where
    minimize' f n = ite (f n) n (minimize' f (Succ n))

instance Show Nat where
  show = show . nat2int

----------------------------------------------------------------------------------------------------

nat2int :: Nat -> Integer
nat2int Zero = 0
nat2int (Succ a) = 1 + (nat2int a)

int2nat :: Integer -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

----------------------------------------------------------------------------------------------------

inc :: Nat -> Nat
inc = Succ

dec :: Nat -> Nat
dec (Succ a) = a

add :: Nat -> Nat -> Nat
add a Zero = a
add a (Succ b) = Succ (add a b)

sub :: Nat -> Nat -> Nat
sub a Zero = a
sub (Succ a) (Succ b) = sub a b

lt :: Nat -> Nat -> Bool
lt a b = not (gte a b)

gt :: Nat -> Nat -> Bool
gt a b = lt b a

lte :: Nat -> Nat -> Bool
lte Zero b = True
lte (Succ a) Zero = False
lte (Succ a) (Succ b) = lte a b

gte :: Nat -> Nat -> Bool
gte a b = lte b a

output :: String
output = show num

num :: Nat
num = minimize func

func :: Nat -> Bool
func n = gt n (int2nat 10)