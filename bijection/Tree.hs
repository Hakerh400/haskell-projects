module Tree where

import Base

data Tree =
  Leaf N |
  Node Tree Tree

instance Eq Tree where
  Leaf a == Leaf b = a == b
  Node l1 r1 == Node l2 r2 = l1 == l2 && r1 == r2
  _ == _ = False

instance Show Tree where
  show (Leaf a) = show a
  show (Node l r) = concat ["(", show l, " ", show r, ")"]