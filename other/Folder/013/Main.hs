{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

main :: IO ()
main = putStrLn output

u :: a
u = undefined

----------------------------------------------------------------------------------------------------

data Map :: * -> * -> * where
  Map :: (a -> b) -> Map a b

get :: forall a b. (Eq a) => Map a b -> a -> b
get (Map f) x = f x

set :: forall a b. (Eq a) => Map a b -> a -> b -> Map a b
set (Map f) x y = Map (set' f x y) where
  set' :: (a -> b) -> a -> b -> a -> b
  set' f x y z = if z == x then y else f z

cmap :: b -> Map a b
cmap = Map . const

----------------------------------------------------------------------------------------------------

map1 :: Map Integer String
map1 = cmap []

map2 = set map1 5 "abc"
map3 = set map2 5 "abcde"

output :: String
output = get map3 5