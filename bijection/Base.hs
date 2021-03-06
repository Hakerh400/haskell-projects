module Base where

import Data.Functor.Identity

type N = Integer

instance MonadFail Identity where
  fail = error

len :: [a] -> N
len xs = toInteger $ length xs

dec :: N -> N
dec a = a - 1

dbg :: (Show a) => a -> b
dbg = error . show

u :: a
u = undefined