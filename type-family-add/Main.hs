{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Kind

data A = A
data B a = B a

instance Show A where
  show _ = ""

instance (Show a) => Show (B a) where
  show _ = '1' : show (u :: a)

type family F a b where
  F a A = a
  F a (B b) = B (F a b)
  F _ _ = A

f :: (Show (F a b)) => a -> b -> F a b -> Int
f _ _ = length . show

main = do
  print $ f (B$B$A) (B$B$B$A) u

u = undefined