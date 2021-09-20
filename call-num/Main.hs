{-# LANGUAGE FlexibleInstances #-}

main :: IO ()
main = 123 print

instance Num ((Integer -> a) -> a) where
  (+) = u
  (*) = u
  abs = u
  signum = u
  negate = u
  fromInteger n f = f n

u = undefined