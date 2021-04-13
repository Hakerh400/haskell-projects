module Program (
  run
) where

import Prelude hiding (reverse)

type Function = (->)
data Bit = Bit0 | Bit1
data List a = Empty | List a (List a)

type Bin = List Bit
type Program = Bin -> Bin

-------------------------------------------------------

caseBit :: Bit -> a -> a -> a
caseBit val f0 f1 = case val of
  Bit0 -> f0
  Bit1 -> f1

caseList :: List a -> b -> (a -> List a -> b) -> b
caseList val f0 f1 = case val of
  Empty -> f0
  List x0 x1 -> f1 x0 x1

-------------------------------------------------------

main :: Bin -> Bin
main a = reverse a

reverse :: Bin -> Bin
reverse a = reverse1 a Empty where
  reverse1 :: Bin -> Bin -> Bin
  reverse1 a b = caseList a f0 f1 where
    f0 = b
    f1 x xs = reverse1 xs (List x b)

-------------------------------------------------------

prog :: Program
prog = main

run :: String -> String
run input = bin2str $ runProg prog $ str2bin input

runProg :: Program -> Bin -> Bin
runProg prog input = prog input

str2bin :: String -> Bin
str2bin [] = Empty
str2bin (x:xs) = List bit (str2bin xs) where
  bit = case x of
    '0' -> Bit0
    '1' -> Bit1

bin2str :: Bin -> String
bin2str Empty = []
bin2str (List x xs) = bit : bin2str xs where
  bit = case x of
    Bit0 -> '0'
    Bit1 -> '1'