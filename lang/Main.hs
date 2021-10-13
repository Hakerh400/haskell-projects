{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Data.Char

import qualified Nat
import qualified List
import qualified Char
import qualified String
import qualified PolyList

import qualified Examples.Codidact.P279147.Program as Prog
import qualified Examples.Codidact.P279147.Test as Tests

input :: String
input = "123 45"

main :: IO ()
main = do
  if run_tests Prog Test
    then putStrLn "ok"
    else putStrLn "ERROR"

type family HeadArg a where
  HeadArg (a -> b) = a

type family TailArgs a where
  TailArgs (a -> b) = b

type family ProgToPolyList a where
  ProgToPolyList (a -> b) = a ': ProgToPolyList b

class Program p where
  apply_arg :: p -> HeadArg p -> TailArgs

run_tests :: (Program p) => p -> [ProgToPolyList p] -> Bool
run_tests prog [] = True
run_tests prog (x:xs) = run_test prog x && run_tests prog xs

run_test :: (Program p) => p -> ProgToPolyList p -> Bool

str_from_hs :: String -> String.String
str_from_hs = String.str_lit

str_to_hs :: String.String -> String
str_to_hs = List.list_exa (\x -> const (char_to_hs x:)) []

char_to_hs :: Char.Char -> Char
char_to_hs = chr . Nat.nat_exa (const (+1)) 0