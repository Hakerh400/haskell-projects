import Prelude hiding (Real, sqrt, recip, splitAt)

type N = Integer
type Real = N -> Bool

realToList :: Real -> [Bool]
realToList r = f 0 where
  f i = r i : f (i + 1)

listToReal :: [Bool] -> Real
listToReal (x:_) 0 = x
listToReal (_:xs) n = listToReal xs $ n - 1

realToDigits :: Int -> Real -> String
realToDigits n r = take n $ realToList r >>= show.fromEnum

asList :: ([Bool] -> [Bool]) -> Real -> Real
asList = (listToReal.).(.realToList)

asList2 :: ([Bool] -> [Bool] -> [Bool]) -> Real -> Real -> Real
asList2 f r1 r2 = listToReal $ f (realToList r1) (realToList r2)

bisect :: (N -> Bool) -> N -> N -> N
bisect f k1 k2
  | k2 == k1 = k1
  | k2 == k1 + 1 = if f k1 then k1 else k2
  | otherwise = if f k3 then bisect f k1 k3 else bisect f k3 k2
  where k3 = div (k1 + k2) 2

bisect1 :: (N -> Bool) -> N -> N -> N
bisect1 f k1 k2 = bisect (not.f) k1 (k2 + 1) - 1

intSqrt :: N -> N
intSqrt n = bisect1 ((<=n).(^2)) 0 n

sqrt :: N -> Real
sqrt n 0 = odd $ intSqrt $ n * 4
sqrt n i = sqrt (n * 4) (i - 1)

recip :: N -> Real
recip n i = odd $ div (2 ^ (i + 1)) n

add :: Real -> Real -> Real
add = asList2 addAsList

sub :: Real -> Real -> Real
sub a = add a . invert

addAsList :: [Bool] -> [Bool] -> [Bool]
addAsList a b = let
  n = findCommon0 (tail a) (tail b) + 1
  (a1, a2) = splitAt n a
  (b1, b2) = splitAt n b
  s = addLists a1 b1
  in s ++ addAsList a2 b2

addLists :: [Bool] -> [Bool] -> [Bool]
addLists xs ys = fst $ foldr f ([], False) $ zip xs ys where
  f (x, y) (acc, carry) = let
    b = (x /= y) /= carry
    carry' = (x && y) || (x && carry) || (y && carry)
    in (b:acc, carry')

findCommon0 :: [Bool] -> [Bool] -> N
findCommon0 (False:_) (False:_) = 0
findCommon0 (_:xs) (_:ys) = 1 + findCommon0 xs ys

invert :: Real -> Real
invert = (not.)

splitAt :: N -> [a] -> ([a], [a])
splitAt 0 xs = ([], xs)
splitAt n (x:xs) = let
  r = splitAt (n - 1) xs
  a1 = fst r
  a2 = snd r
  in (x:a1, a2)

main :: IO ()
main = do
  putStrLn $ realToDigits 80 $ sqrt 7
  putStrLn $ realToDigits 80 $ sqrt 5
  putStrLn $ ""
  putStrLn $ "01101000111000010000000111000111011000001000100001"
  putStrLn $ ""
  putStrLn $ realToDigits 80 $ sub (sqrt 7) (sqrt 5)
