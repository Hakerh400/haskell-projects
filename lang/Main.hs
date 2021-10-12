import Data.Char

import qualified Nat
import qualified List
import qualified Char
import qualified String
import qualified Program

input :: String
input = "123 45"

main :: IO ()
main = do
  putStrLn $ str_to_hs $ Program.main $ str_from_hs $ input

str_from_hs :: String -> String.String
str_from_hs = foldr (List.cons . char_from_hs) List.nil

str_to_hs :: String.String -> String
str_to_hs = List.list_exa (\x -> const (char_to_hs x:)) []

char_from_hs :: Char -> Char.Char
char_from_hs = go . ord where
  go 0 = Nat.zero
  go n = Nat.suc $ go $ n - 1

char_to_hs :: Char.Char -> Char
char_to_hs = chr . Nat.nat_exa (const (+1)) 0