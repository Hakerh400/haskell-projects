class Array impl where
  end :: impl a
  arr :: a -> impl a -> impl a
  match :: impl a -> b -> (a -> impl a -> b) -> b

data Array1 a =
  End1 |
  Arr1 a (Array1 a)

data Array2 a =
  End2 |
  Arr2 a (Array2 a)

instance Array Array1 where
  end = End1
  arr a b = Arr1 a b
  match End1 f g = f
  match (Arr1 x xs) f g = g x xs

instance Array Array2 where
  end = End2
  arr a b = Arr2 a b
  match End2 f g = f
  match (Arr2 x xs) f g = g x xs

instance Array [] where
  end = []
  arr = (:)
  match [] f g = f
  match (x:xs) f g = g x xs

type MyArr = []

len :: Integer
len = 10

main :: IO ()
main = putStrLn output

output :: String
output = stringify $ myArr

myArr :: MyArr Integer
myArr = ca id len

ca :: Array a => (Integer -> b) -> Integer -> a b
ca f n = ca1 f n 0 where
  ca1 :: Array a => (Integer -> b) -> Integer -> Integer -> a b
  ca1 f n i
    | i == n = end
    | otherwise = arr (f i) (ca1 f n (i + 1))

stringify :: Array a => Show b => a b -> String
stringify a = match a "[]" ((("["++).).(((++"]").). elemAndRest)) where
  stringify1 :: Array a => Show b => a b -> String
  stringify1 a = match a "" (((", "++).). elemAndRest)
  elemAndRest :: Array a => Show b => b -> a b -> String
  elemAndRest elem rest = show elem ++ stringify1 rest