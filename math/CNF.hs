module CNF
  ( CNF(..)
  , Clause(..)
  , Item(..)
  , pred2cnf
  , isCnfProved
  , invItem
  , invItemSign
  , cnf2clauses
  , item2expr
  , mapItem
  , isClauseTaut
  , getItemSign
  , clause2set
  , substZippedItems
  , substIdentClause
  , substIdentClause'
  , mapClause
  ) where

import qualified Data.List as List

import qualified Data.Set as Set
import Data.Set (Set)

import Predicate
import Expression
import Util

newtype CNF = CNF (Set Clause)

newtype Clause = Clause (Set Item)
  deriving (Eq)

data Item = Item ItemSign Expr
  deriving (Eq, Ord)

data ItemSign = ItemP | ItemN
  deriving (Eq, Ord)

instance Ord Clause where
  compare (Clause set1) (Clause set2) = let
    size1 = Set.size set1
    size2 = Set.size set2
    in case compare size1 size2 of
      EQ -> compare set1 set2
      a  -> a

instance Show CNF where
  show (CNF conjs) = conjs2str conjs

instance Show Item where
  show (Item sign a) = show sign ++ show a

instance Show ItemSign where
  show ItemP = ""
  show ItemN = "~"

conjs2str :: Set (Clause) -> String
conjs2str = numLines . map disj2str . Set.toList

disj2str :: Clause -> String
disj2str = ss disjSep . map show . Set.toList . clause2set

clause2set :: Clause -> Set Item
clause2set (Clause a) = a

pred2cnf :: Pred -> CNF
pred2cnf p = CNF $
  Set.filter (not . isClauseTaut) $
  pred2cnfConj p

pred2cnfConj :: Pred -> Set (Clause)
pred2cnfConj (And a b) = pred2cnfConj a `Set.union` pred2cnfConj b
pred2cnfConj a         = if predHasTrue a
  then Set.empty
  else Set.singleton $ Clause $ pred2cnfDisj a

pred2cnfDisj :: Pred -> Set Item
pred2cnfDisj (Or a b) = pred2cnfDisj a `Set.union` pred2cnfDisj b
pred2cnfDisj Pfalse   = Set.empty
pred2cnfDisj a        = Set.singleton $ pred2Item a

pred2Item :: Pred -> Item
pred2Item (Pnot (Stat a)) = Item ItemN a
pred2Item (Stat a)        = Item ItemP a
pred2Item a = error $ show a

isClauseTaut :: Clause -> Bool
isClauseTaut clause = let
  set = clause2set clause
  in any (\a -> invItem a `elem` set) set

getItemSign :: Item -> ItemSign
getItemSign (Item sign _) = sign

isCnfProved :: CNF -> Bool
isCnfProved (CNF set) = Clause Set.empty `elem` set

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

substIdentClause :: String -> Expr -> Set Item -> Set Item
substIdentClause a b = mapSet . mapItem $ substIdentE a b

substIdentClause' :: (String, Expr) -> Set Item -> Set Item
substIdentClause' = uncurry substIdentClause

mapClause :: (Set Item -> Set Item) -> Clause -> Clause
mapClause f (Clause a) = Clause $ f a