{-# LANGUAGE ExplicitForAll, ScopedTypeVariables #-}

import Data.Bits
import Prelude hiding (min, max, repeat, take, reverse, zip, sort, remove)

main :: IO ()
main = putStrLn output

output :: String
output = show $ sort list

list :: [Integer]
list = [3,5,7,9,2,4,3,6,8,2,0,7,1,3,8]

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (elem:[]) = [elem]
sort original = smallest : sort (remove original smallest) where
  (elem:rest) = original
  smallest = min original

min :: (Ord a) => [a] -> a
min (a:[]) = a
min (a:b)
  | a < c = a
  | otherwise = c
  where
    c = min b

remove :: (Eq a) => [a] -> a -> [a]
remove (a:b) c
  | a == c = b
  | otherwise = a : remove b c