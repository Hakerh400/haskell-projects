{-# LANGUAGE GADTs, ScopedTypeVariables #-}

import Prelude hiding (map)

import Program as P
import CustomList

main :: IO ()
main = putStrLn output

prog :: Bin -> Bin
prog a = invert a where
  invert :: Bin -> Bin
  invert a = map a invertBit
  invertBit :: Bit -> Bit
  invertBit a = case a of
    B0 -> B1
    B1 -> B0

input :: String
input = "1011"

output :: String
output = bin2str $ run (Const prog) $ str2bin input