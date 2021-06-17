module Predicate
  ( Pred(..)
  , Expr(..)
  , builtinPredNames
  , isBuiltinPred
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
  Or     Pred   Pred |
  And    Pred   Pred |
  Pnot   Pred        |
  Stat   Expr        |
  Ptrue              |
  Pfalse
  deriving (Eq, Ord)

data Expr =
  ExprI String |
  ExprP Expr Expr
  deriving (Eq, Ord)

instance Show Pred where
  show (Forall a b) = concat ["V", a, " ", show b]
  show (Exists a b) = concat ["E", a, " ", show b]
  show (Impl   a b) = op "->" a b
  show (Equiv  a b) = op "<->" a b
  show (Or     a b) = op "v" a b
  show (And    a b) = op "^" a b
  show (Pnot   a  ) = "~" ++ show a
  show (Stat   a  ) = expr2str True a
  show (Ptrue     ) = "True"
  show (Pfalse    ) = "False"

instance Show Expr where
  show = expr2str False

builtinPredNames :: [String]
builtinPredNames =
  [ "all"
  , "exi"
  , "->"
  , "<->"
  , "|"
  , "&"
  , "~"
  , "True"
  , "False"
  ]

isBuiltinPred :: String -> Bool
isBuiltinPred = (`elem` builtinPredNames)

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
op a b c = parens $ sp [show b, a, show c]