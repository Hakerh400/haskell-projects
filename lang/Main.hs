import Data.Char

import Nat
import List
import qualified Program as P

inp :: String
inp = "123 45"

out :: String
out = plist2str $ P.main $ str2plist inp

str2plist :: String -> List Nat
str2plist = foldr (\x xs -> list_cons (char2pnat x) xs) list_nil

plist2str :: List Nat -> String
plist2str = list_exa (\x _ xs -> pnat2char x : xs) []

char2pnat :: Char -> Nat
char2pnat = int2pnat . ord

pnat2char :: Nat -> Char
pnat2char = chr . pnat2int

int2pnat :: Int -> Nat
int2pnat 0 = nat_zero
int2pnat n = nat_suc $ int2pnat $ n - 1

pnat2int :: Nat -> Int
pnat2int = nat_exa (\_ n -> n + 1) 0

main :: IO ()
main = putStrLn out