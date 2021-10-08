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

type family FGType t a b r where
  FGType a b r String = (String -> r) -> (a -> b) -> r
  FGType a b r t = (String -> a) -> (t -> r) -> r

func :: forall a b r t. t -> FGType a b r t
func a = undefined

f1 :: String -> String
f1 a = "String: " ++ show a

f2 :: Int -> Int
f2 a = a + 5

main :: IO ()
main = do
  let f a = func a f1 f2
  print $ f "abc"
  print $ f 7