module Predicate (
  Pred(..),
  Expr(..)
) where

import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)

import Util
import State

data Pred =
  Forall String Pred |
  Exists String Pred |
  Impl   Pred   Pred |
  Equiv  Pred   Pred |
  Disj   Pred   Pred |
  Conj   Pred   Pred |
  Neg    Pred        |
  Stat   Expr        |
  PTrue              |
  PFalse
  deriving (Eq, Ord)

data Expr =
  ExprI String |
  ExprP Expr Expr
  deriving (Eq, Ord)

instance Show Pred where
  show (Forall a b) = concat ["V", a, show b]
  show (Exists a b) = concat ["E", a, show b]
  show (Impl   a b) = op "->" a b
  show (Equiv  a b) = op "<->" a b
  show (Disj   a b) = op "v" a b
  show (Conj   a b) = op "^" a b
  show (Neg    a)   = "~"  ++ show a
  show (Stat   a)   = "\\" ++ expr2str True a

instance Show Expr where
  show = expr2str False

expr2str :: Bool -> Expr -> String
expr2str ps expr = case expr of
  (ExprI a)   -> a
  (ExprP a b) -> let
    a1 = expr2str False a
    b1 = expr2str True  b
    c  = sp [a1, b1]
    in if ps
      then parens c
      else c

op :: String -> Pred -> Pred -> String
op a b c = parens $ sp [a, show b, show c]