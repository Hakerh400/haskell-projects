module Base where

import Data.Functor.Identity

type N = Integer

instance MonadFail Identity where
  fail = error

dec :: N -> N
dec a = a - 1