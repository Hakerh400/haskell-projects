{-# LANGUAGE NoImplicitPrelude, GADTs, ScopedTypeVariables #-}

module Program (
  Program,
  run,
  str2bin,
  bin2str
) where

import Prelude (String, Char)
import CustomList

----------------------------------------------------------------------------------------------------

data Bit where
  B0 :: Bit
  B1 :: Bit

data List a where
  Empty :: List a
  Cons :: a -> List a -> List a

run :: Program -> Bin -> Bin
run prog inp = prog inp

----------------------------------------------------------------------------------------------------

instance CustomList List where
  empty = Empty
  cons = Cons
  pat a b c = case a of
    Empty -> b
    Cons x y -> c x y

type Bin = List Bit
type Program = Bin -> Bin

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