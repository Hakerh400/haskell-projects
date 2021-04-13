module System (
  A,
  f
) where

import Data.List

data Term =
  Zero |
  Succ |
  Nat
  deriving (Show, Eq)

data A =
  B Term |
  C A A

instance Show A where
  show (B Zero) = "0"
  show (C (B Succ) a) = show $ 1 + (nat2int a)
  show (C (B Nat) a) = triple (show a) "::" "Nat"

instance Eq A where
  B a == B b = a == b
  B a == _ = False
  C a b == C c d = a == c && b == d
  C a b == _ = False

f :: String -> [A] -> A

-- Natural numbers
f "zero" [] = B Zero
f "succ" (C (B Nat) a : []) = C (B Succ) a
f "nat" (a@(B Zero) : []) = C (B Nat) a
f "nat" (a@(C (B Succ) _) : []) = C (B Nat) a

-- f ""

f op args = error $ op ++ " " ++ intercalate " " (map inspect args)

inspect :: A -> String
inspect (B a) = show a
inspect (C a b) = pair (inspect a) (inspect b)

parens :: [String] -> String
parens a = "(" ++ intercalate " " a ++ ")"

pair :: String -> String -> String
pair a b = parens [a, b]

triple :: String -> String -> String -> String
triple a b c = parens [a, b, c]

nat2int :: A -> Integer
nat2int (B Zero) = 0
nat2int (C (B Succ) a) = 1 + (nat2int a)