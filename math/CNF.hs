module CNF
  ( CNF(..)
  , Item(..)
  , pred2cnf
  , isCnfProved
  , invItem
  , invItemSign
  , cnf2clauses
  , item2expr
  , mapItem
  , isClauseTaut
  , substZippedItems
  , substIdentClause
  , substIdentClause'
  ) where

import qualified Data.List as List

import qualified Data.Set as Set
import Data.Set (Set)

import Predicate
import Expression
import Util

type Clause = Set Item

newtype CNF = CNF (Set Clause)

data Item = Item ItemSign Expr
  deriving (Eq, Ord)

data ItemSign = ItemP | ItemN
  deriving (Eq, Ord)

instance Show Item where
  show (Item sign a) = show sign ++ show a

instance Show ItemSign where
  show ItemP = ""
  show ItemN = "~"

instance Show CNF where
  show (CNF conjs) = conjs2str conjs

conjs2str :: Set (Clause) -> String
conjs2str = numLines . map disj2str . Set.toList

disj2str :: Clause -> String
disj2str = ss disjSep . map show . Set.toList

pred2cnf :: Pred -> CNF
pred2cnf p = CNF $
  Set.filter (not . isClauseTaut) $
  pred2cnfConj p

pred2cnfConj :: Pred -> Set (Clause)
pred2cnfConj (And a b) = pred2cnfConj a `Set.union` pred2cnfConj b
pred2cnfConj a         = if predHasTrue a
  then Set.empty
  else Set.singleton $ pred2cnfDisj a

pred2cnfDisj :: Pred -> Clause
pred2cnfDisj (Or a b) = pred2cnfDisj a `Set.union` pred2cnfDisj b
pred2cnfDisj Pfalse   = Set.empty
pred2cnfDisj a        = Set.singleton $ pred2Item a

pred2Item :: Pred -> Item
pred2Item (Pnot (Stat a)) = Item ItemN a
pred2Item (Stat a)        = Item ItemP a
pred2Item a = error $ show a

isClauseTaut :: Clause -> Bool
isClauseTaut disjs = any
  (\a -> invItem a `elem` disjs)
  disjs

isCnfProved :: CNF -> Bool
isCnfProved (CNF set) = Set.empty `elem` set

invItem :: Item -> Item
invItem (Item sign expr) = Item (invItemSign sign) expr

invItemSign :: ItemSign -> ItemSign
invItemSign ItemP = ItemN
invItemSign ItemN = ItemP

predHasTrue :: Pred -> Bool
predHasTrue (Or a b) = predHasTrue a || predHasTrue b
predHasTrue Ptrue    = True
predHasTrue _        = False

disjSep :: String
disjSep = concat [s2, "|", s2]

cnf2clauses :: CNF -> Set (Clause)
cnf2clauses (CNF a) = a

item2expr :: Item -> Expr
item2expr (Item _ a) = a

mapItem :: (Expr -> Expr) -> Item -> Item
mapItem f (Item sign a) = Item sign $ f a

substZippedItems :: (String, String) -> [Item] -> [Item]
substZippedItems = map . substZippedItem

substZippedItem :: (String, String) -> Item -> Item
substZippedItem = mapItem . substZippedExpr

substIdentClause :: String -> Expr -> Clause -> Clause
substIdentClause a b = mapSet . mapItem $ substIdentE a b

substIdentClause' :: (String, Expr) -> Clause -> Clause
substIdentClause' = uncurry substIdentClause