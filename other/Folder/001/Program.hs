module Program (
  run
) where

type Function = (->)
data Bit = Bit0 | Bit1
data List a = Empty | Elem a (List a)
data A3 a b c d =
  A3_0 (List ((A3 (A3 b (a -> c) A4) A4 a) -> List (List (A3 d)) -> List a))
data A4 = A4

a0 :: Bit
a0 = Bit0
a1 :: Bit
a1 = Bit1
a2 :: Bit -> a -> a -> a
a2 val f0 f1 = case val of
  Bit0 -> f0
  Bit1 -> f1
a3 :: List a
a3 = Empty
a4 :: a -> List a -> List a
a4 = Elem
a5 :: List a -> b -> (a -> List a -> b) -> b
a5 val f0 f1 = case val of
  Empty -> f0
  Elem a b -> f1 a b
a6 :: List Bit -> List Bit
a6 = a8 a7

a7 :: _
a8 :: _ -> List Bit -> List Bit

prog :: Main
prog = a6

run :: String -> String
run input = bin2str $ runProg prog $ str2bin input

runProg :: Main -> List Bit -> List Bit
runProg prog input = prog input

str2bin :: String -> List Bit
str2bin [] = Empty
str2bin (x:xs) = Elem bit (str2bin xs) where
  bit = case x of
    '0' -> Bit0
    '1' -> Bit1

bin2str :: List Bit -> String
bin2str Empty = []
bin2str (Elem x xs) = bit : bin2str xs where
  bit = case x of
    Bit0 -> '0'
    Bit1 -> '1'