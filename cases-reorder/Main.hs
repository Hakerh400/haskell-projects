{-# LANGUAGE ScopedTypeVariables #-}

data A = A Integer deriving (Eq, Show)

instance Num A where
  _ + _ = 0
  _ * _ = 0
  abs _ = 0
  signum _ = 0
  negate _ = 0
  fromInteger 0 = A 0
  fromInteger 1 = undefined
  fromInteger 2 = A 2

main = print $ func $ 2

func :: A -> A
func 0 = 0
func 2 = 2
func 1 = undefined
-- func 2 = 2