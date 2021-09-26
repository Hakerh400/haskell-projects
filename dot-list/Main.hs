{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

import Data.Kind

data DotList :: * -> * -> * where
  Nil :: DotList a a
  Dot :: DotList a b -> (b -> c) -> DotList a c

(!) :: DotList a b -> (b -> c) -> DotList a c
(!) = Dot

toFunc :: DotList a b -> a -> b
toFunc Nil = id
toFunc (Dot xs x) = x . toFunc xs

main = do
  print $ out

out = toFunc f [123, 45]

f = Nil
  ! sum
  ! show
  ! drop 1
  ! (read :: String -> Int)
  ! (+2)