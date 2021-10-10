{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

type family GetFuncType t x y r where
  GetFuncType String x y r = '(String, (String -> r) -> (x -> y) -> r)
  GetFuncType t x y r = '(t, (String -> y) -> (t -> r) -> r)

type family Fst a where
  Fst '(a, b) = a

type family Snd a where
  Snd '(a, b) = b

class (ft ~ Snd (GetFuncType t x y r)) => Class t ft x y r where
  method :: t -> ft

instance Class String ((String -> r) -> (x -> y) -> r) x y r where
  method str f1 f2 = f1 str

instance (Snd (GetFuncType t x y r) ~ ((String -> y) -> (t -> r) -> r)) => Class t ((String -> y) -> (t -> r) -> r) x y r where
  method val f1 f2 = f2 val

-- func :: String -> (String -> r) -> (x -> y) -> r
-- func :: t -> (String -> y) -> (t -> r) -> r
func :: forall t ft x y r. (ft ~ Snd (GetFuncType t x y r), Class t ft x y r) => t -> Snd (GetFuncType t x y r)
func a = method @t @ft @x @y @r a

f1 :: String -> String
f1 a = "String: " ++ a

f2 :: Int -> Int
f2 a = a + 5

main :: IO ()
main = do
  let f a = func a f1 f2
  print $ f "abc"
  print $ f (5 :: Int)