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
  Equiv  (Set Pred)  |
  Impl   (Set Pred)  |
  Disj   (Set Pred)  |
  Conj   (Set Pred)  |
  Neg    Pred        |
  Stat   Expr        |
  PTrue              |
  PFalse

data Expr =
  Expr String [Expr]

instance Show Pred where
  show = inspectToList inspectPred

instance Show Expr where
  show = inspectToList inspectExpr

inspectPred :: Pred -> [String]
inspectPred (Forall a p) = ["all", a, show p]
inspectPred (Exists a p) = ["exi", a, show p]
inspectPred (Equiv  ps)  = "<->" : inspectPredSet ps
inspectPred (Impl   ps)  = "->" : inspectPredSet ps
inspectPred (Disj   ps)  = "|" : inspectPredSet ps
inspectPred (Conj   ps)  = "&" : inspectPredSet ps
inspectPred (Neg    p)   = ["~", show p]
inspectPred (Stat   e)   = "." : inspectExpr e
inspectPred PTrue        = ["T"]
inspectPred PFalse       = ["F"]

inspectPredSet :: Set Pred -> [String]
inspectPredSet = map show . Set.toList

inspectExpr :: Expr -> [String]
inspectExpr (Expr a es) = a : map show es

inspectToList :: (a -> [String]) -> a -> String
inspectToList f a = concat ["(", List.intercalate " " $ f a, ")"]