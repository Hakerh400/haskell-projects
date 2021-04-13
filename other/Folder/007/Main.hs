{-# LANGUAGE ViewPatterns #-}

import qualified Data.IntMap as I
import qualified Data.List.NonEmpty as N
import System.IO

data Term
  = V Int
  | S
  | K
  | I
  | A (N.NonEmpty (Int, Term, Term))
  deriving (Show, Eq, Ord)

parse :: String -> (Term, String)
parse = parseApp . parse1

parseApp :: (Term, String) -> (Term, String)
parseApp (t, ' ':s) = parseApp (t, s)
parseApp (t, "") = (t, "")
parseApp (t, ')':s) = (t, ')' : s)
parseApp (t1, parse1 -> (t2, s)) =
  parseApp (A (pure (appLen (t1, t2), t1, t2)), s)

parse1 :: String -> (Term, String)
parse1 (' ':s) = parse1 s
parse1 ('(':(parse -> (t, ')':s))) = (t, s)
parse1 ('S':s) = (S, s)
parse1 ('K':s) = (K, s)
parse1 ('I':s) = (I, s)
parse1 (lex -> [(i, s)]) = (V (read i), s)

ruleStrings :: [(String, String)]
ruleStrings =
  [ ("1 3(2 3)", "S1 2 3")
  , ("S(K(S(K1)))(S(K(S(K2)))3)", "S(K(S(K(S(K1)2))))3")
  , ("S(K(S(K1)))(S(K2))", "S(K(S(K1)2))")
  , ("S(K1)(K2)", "K(1 2)")
  , ("S(K1)I", "1")
  , ("S(S(K1)2)(K3)", "S(K(S1(K3)))2")
  , ("S(SI1)I", "S(SSK)1")
  ]

rules :: [(Term, Term)]
rules = [(a, b) | (parse -> (a, ""), parse -> (b, "")) <- ruleStrings]

len :: Term -> Int
len (V _) = 1
len S = 1
len K = 1
len I = 3
len (A ((l, _, _) N.:| _)) = l

appLen :: (Term, Term) -> Int
appLen (t1, S) = len t1 + 1
appLen (t1, K) = len t1 + 1
appLen (K, I) = 2
appLen (t1, t2) = len t1 + len t2 + 2

notA :: Term -> Bool
notA (A _) = False
notA _ = True

alt :: N.NonEmpty Term -> Term
alt ts =
  head $
  N.filter notA ts ++
  [A (N.nub (a N.:| filter (\(l, _, _) -> l <= minLen + 3) aa))]
  where
    a@(minLen, _, _) N.:| aa =
      N.sort $ do
        A b <- ts
        b

match :: Term -> Term -> I.IntMap Term -> [I.IntMap Term]
match (V i) t m =
  case I.lookup i m of
    Just ((/= t) -> True) -> []
    _ -> [I.insert i t m]
match S S m = [m]
match K K m = [m]
match I I m = [m]
match (A a) (A a') m = do
  (_, t1, t2) <- N.toList a
  (_, t1', t2') <- N.toList a'
  m1 <- match t1 t1' m
  match t2 t2' m1
match _ _ _ = []

sub :: I.IntMap Term -> Term -> Term
sub _ S = S
sub _ K = K
sub _ I = I
sub m (V i) = m I.! i
sub m (A a) =
  alt $ do
    (_, t1, t2) <- a
    pure (sub m t1 & sub m t2)

optimize :: Term -> Term
optimize t = alt $ t N.:| [sub m b | (a, b) <- rules, m <- match a t I.empty]

infixl 5 &

(&) :: Term -> Term -> Term
t1 & t2 = optimize (A (pure (appLen (t1, t2), t1, t2)))

elim :: Int -> Term -> Term
elim n (V ((== n) -> True)) = I
elim n (A a) =
  alt $ do
    (_, t1, t2) <- a
    pure (S & elim n t1 & elim n t2)
elim _ t = K & t

paren :: String -> Bool -> String
paren s True = "(" ++ s ++ ")"
paren s False = s

output :: Term -> Bool -> String
output S = const "S"
output K = const "K"
output I = paren "SKK"
output (V i) = \_ -> show i ++ " "
output (A ((_, K, I) N.:| _)) = paren "SK"
output (A ((_, t1, t2) N.:| _)) = paren (output t1 False ++ output t2 True)

convert :: Int -> Term -> Term
convert 0 t = t
convert n t = convert (n - 1) (elim n t)

process :: String -> String
process (lex -> [(n, lex -> [((`elem` ["=", "->"]) -> True, parse -> (t, ""))])]) =
  output (convert (read n) t) False

main :: IO ()
main = do
  line <- getLine
  putStrLn (process line)
  hFlush stdout
  main