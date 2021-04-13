{-# LANGUAGE NoImplicitPrelude, GADTs, ScopedTypeVariables #-}

module Program (
  Term(..),
  Bit(..),

  Program,
  Bin,

  run,
  str2bin,
  bin2str
) where

import Prelude (String, Char, error)
import CustomList

----------------------------------------------------------------------------------------------------

data Term x where
  Const :: a -> Term a
  K :: Term (a -> b -> a)
  S :: Term ((a -> b -> c) -> (a -> b) -> a -> c)
  Call :: Term (a -> b) -> Term a -> Term b

data Bit where
  B0 :: Bit
  B1 :: Bit

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data List x where
  Empty :: List a
  Cons :: a -> List a -> List a

run :: Program -> Bin -> Bin
run prog input = eval (Call prog (Const input))

k :: a -> b -> a
k a b = a

s :: (a -> b -> c) -> (a -> b) -> a -> c
s a b c = a c (b c)

eval :: Term a -> a
eval a = case a of
  Const a -> a
  K -> k
  S -> s
  Call a b -> (eval a) (eval b)

----------------------------------------------------------------------------------------------------

instance CustomList List where
  empty = Empty
  cons = Cons
  pat a b c = case a of
    Empty -> b
    Cons x y -> c x y

type Bin = List Bit
type Program = Term (Bin -> Bin)

str2bin :: String -> Bin
str2bin a = map a func where
  func :: Char -> Bit
  func a = case a of
    '0' -> B0
    '1' -> B1

bin2str :: Bin -> String
bin2str a = map a func where
  func :: Bit -> Char
  func a = case a of
    B0 -> '0'
    B1 -> '1'