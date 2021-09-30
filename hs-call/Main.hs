{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

type Type_k = forall a b. a -> b -> a
type Type_s = forall a b c. (a -> b -> c) -> (a -> b) -> a -> c
type Type_iota = forall a. (Type_s -> Type_k -> a) -> a

iota :: Type_iota
iota a = a s k where
  k :: Type_k
  k a b = a
  s :: Type_s
  s a b c = a c (b c)

-- i :: forall a. a -> a
-- i = iota iota

main :: IO ()
main = putStrLn "ok"