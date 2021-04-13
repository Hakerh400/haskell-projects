module System (
  int2ident,
  A,
  f
) where

import Data.Char
import Data.List

import Prelude hiding (join)

data Term =
    Zero 
  | Succ 
  | Nat  
  | Ident
  | Wff
  | Abs
  | All
  deriving (Show, Eq)

data A =
  B Term |
  C A A

instance Show A where
  show (B Zero) = "0"
  show (C (B Succ) a) = show $ 1 + (nat2int a)
  show (C (B Nat) a) = typeof a "Nat"
  show (C (B Ident) a) = int2ident $ nat2int a
  show (C (B Wff) a) = typeof a "Wff"
  show (C (C (B Abs) a) b) = triple "Abs" (show a) (show b)
  show (C (B All) (C (C (B Abs) a) b)) = triple "All" (show a) (show b)
  show a = err "show" [a]

instance Eq A where
  B a == B b = a == b
  B a == _ = False
  C a b == C c d = a == c && b == d
  C a b == _ = False

f :: String -> [A] -> A

-- Natural numbers
f "zero"  [] = B Zero
f "succ"  (C (B Nat) a : []) = C (B Succ) a
f "nat"   (a@(B Zero) : []) = C (B Nat) a
f "nat"   (a@(C (B Succ) _) : []) = C (B Nat) a

f "ident" (C (B Nat) a : []) = C (B Ident) a
f "abs"   (a@(C (B Ident) _) : C (B Wff) b : []) = C (C (B Abs) a) b
f "all"   (a@(C (C (B Abs) _) _) : []) = C (B All) a
f "wff"   (a@(C (B Ident) _) : []) = C (B Wff) a
f "wff"   (a@(C (B All) _) : []) = C (B Wff) a

f a b = err ("f " ++ a) b

inspect :: A -> String
inspect (B a) = show a
inspect (C a b) = pair (inspect a) (inspect b)

parens :: [String] -> String
parens a = join ["(", intercalate " " a, ")"]

pair :: String -> String -> String
pair a b = parens [a, b]

triple :: String -> String -> String -> String
triple a b c = parens [a, b, c]

nat2int :: A -> Integer
nat2int (B Zero) = 0
nat2int (C (B Succ) a) = 1 + (nat2int a)

int2ident :: Integer -> String
int2ident n = int2ident' (n + 1) where
  int2ident' :: Integer -> String
  int2ident' 0 = []
  int2ident' n = rest ++ [letter] where
    letter = chr (97 + (fromIntegral $ (n - 1) `mod` 26))
    rest = int2ident' ((n - 1) `div` 26)

err :: String -> [A] -> a
err a b = error $ (join $ "\n\n" : a : " " : map inspect b) ++ "\n"

join :: [String] -> String
join = intercalate ""

typeof :: A -> String -> String
typeof a b = triple (show a) "::" b