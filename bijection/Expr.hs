module Expr
  ( Expr(..)
  ) where

import Base
import Tree
import TreeSer
import Comb

data Expr
  = ExprNat N
  | ExprTree Tree
  | ExprComb Comb [Expr]