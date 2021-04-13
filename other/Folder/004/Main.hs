{-# LANGUAGE GADTs, ScopedTypeVariables #-}

import qualified Program as P
import CustomList

main :: IO ()
main = putStrLn output

prog :: P.Program
prog = id

input :: String
input = "1011"

output :: String
output = P.bin2str $ P.run prog $ P.str2bin input