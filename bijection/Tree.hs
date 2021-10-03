{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Tree where

import GHC.Generics (Generic)
import Control.DeepSeq

import Base

type Table = [(Tree, Maybe N)]

data Tree =
  Leaf N |
  Node Tree Tree
  deriving (Generic, NFData)

instance Eq Tree where
  Leaf a == Leaf b = a == b
  Node l1 r1 == Node l2 r2 = l1 == l2 && r1 == r2
  _ == _ = False

instance Show Tree where
  show (Leaf a) = show a
  show (Node l r) = concat ["(", show l, " ", show r, ")"]