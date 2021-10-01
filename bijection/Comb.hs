module Comb
  ( Comb(..)
  , allCombs
  , combsNum
  , combArities
  , combsTable
  ) where

import Base
import Tree

data Comb
  = K
  | S
  | NIL
  | PAIR
  | EXA
  deriving (Bounded, Enum, Ord, Eq, Show)

allCombs :: [Comb]
allCombs = [minBound..maxBound]

combsNum :: N
combsNum = len allCombs

combArities :: [N]
combArities = [2, 3, 0, 2, 3]

combsTable :: Table
combsTable = zipWith combTable' [0..combsNum-1] combArities where
  combTable' i c = (Leaf i, Just c)