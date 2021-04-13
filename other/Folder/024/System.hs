module System (
  OP(..),
  Term(..),
  A,
  f
) where

import Data.Char
import Data.List

import Prelude hiding (join)

data OP =
    F_zero
  | F_succ
  | F_nat
  | F_ident
  | F_abs
  | F_all
  | F_wff
  | F_proof
  deriving (Show, Eq)

data Term =
    T_Zero 
  | T_Succ 
  | T_Nat  
  | T_Ident
  | T_Wff
  | T_Abs
  | T_All
  deriving (Show, Eq)

data A =
  B Term |
  C A A

instance Show A where
  show (B T_Zero) = "0"
  show (C (B T_Succ) a) = show $ 1 + (nat2int a)
  show (C (B T_Nat) a) = typeof a "Nat"
  show (C (B T_Ident) a) = int2ident $ nat2int a
  show (C (B T_Wff) a) = typeof a "Wff"
  show (C (C (B T_Abs) a) b) = triple "Abs" (show a) (show b)
  show (C (B T_All) (C (C (B T_Abs) a) b)) = triple "All" (show a) (show b)
  show a = err "show" [a]

instance Eq A where
  B a == B b = a == b
  B a == _ = False
  C a b == C c d = a == c && b == d
  C a b == _ = False

f :: OP -> [A] -> A

f F_zero  [] = B T_Zero
f F_succ  (C (B T_Nat) a : []) = C (B T_Succ) a
f F_nat   (a@(B T_Zero) : []) = C (B T_Nat) a
f F_nat   (a@(C (B T_Succ) _) : []) = C (B T_Nat) a

f F_ident (C (B T_Nat) a : []) = C (B T_Ident) a
f F_abs   (a@(C (B T_Ident) _) : C (B T_Wff) b : []) = C (C (B T_Abs) a) b
f F_all   (a@(C (C (B T_Abs) _) _) : []) = C (B T_All) a
f F_wff   (a@(C (B T_Ident) _) : []) = C (B T_Wff) a
f F_wff   (a@(C (B T_All) _) : []) = C (B T_Wff) a

f a b = err ("f " ++ show a) b

forall :: Integer -> A -> A
forall n a = f F_all [f F_abs [f F_ident [f F_nat [int2nat n]], f F_wff [a]]]

inspect :: A -> String
inspect (B a) = show a
inspect (C a b) = pair (inspect a) (inspect b)

parens :: [String] -> String
parens a = join ["(", intercalate " " a, ")"]

pair :: String -> String -> String
pair a b = parens [a, b]

triple :: String -> String -> String -> String
triple a b c = parens [a, b, c]

int2nat :: Integer -> A
int2nat 0 = f F_zero []
int2nat n = f F_succ [f F_nat [int2nat (n - 1)]]

nat2int :: A -> Integer
nat2int (B T_Zero) = 0
nat2int (C (B T_Succ) a) = 1 + (nat2int a)

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