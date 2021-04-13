{-# LANGUAGE NoImplicitPrelude, GADTs, ScopedTypeVariables #-}

module Program (
  output
) where

import Prelude (Show, show)

import CustomList

data List a where
  Empty :: List a
  Cons :: a -> List a -> List a

instance CustomList List where
  empty = Empty
  cons = Cons
  pat a b c = case a of
    Empty -> b
    Cons x y -> c x y

data Bit where
  B0 :: Bit
  B1 :: Bit

instance Show Bit where
  show B0 = "0"
  show B1 = "1"

output :: List Bit
output = Cons B0 (Cons B1 Empty)