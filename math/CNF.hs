module CNF
  ( Item
  , CNF
  , pred2cnf
  , isCnfProved
  , invItem
  ) where

import qualified Data.List as List

import qualified Data.Set as Set
import Data.Set (Set)

import Predicate
import Expression
import Util

data Item =
  ItemP Expr |
  ItemN Expr
  deriving (Eq, Ord)

newtype CNF = CNF (Set (Set Item))

instance Show Item where
  show (ItemP a) = show a
  show (ItemN a) = "~" ++ show a

instance Show CNF where
  show (CNF conjs) = conjs2str conjs

conjs2str :: Set (Set Item) -> String
conjs2str = numLines . map disj2str . Set.toList

disj2str :: Set Item -> String
disj2str = ss disjSep . map show . Set.toList

pred2cnf :: Pred -> CNF
pred2cnf p = CNF $
  Set.filter filterDisjs $
  pred2cnfConj p

pred2cnfConj :: Pred -> Set (Set Item)
pred2cnfConj (And a b) = pred2cnfConj a `Set.union` pred2cnfConj b
pred2cnfConj a         = if predHasTrue a
  then Set.empty
  else Set.singleton $ pred2cnfDisj a

pred2cnfDisj :: Pred -> Set Item
pred2cnfDisj (Or a b) = pred2cnfDisj a `Set.union` pred2cnfDisj b
pred2cnfDisj Pfalse   = Set.empty
pred2cnfDisj a        = Set.singleton $ pred2Item a

pred2Item :: Pred -> Item
pred2Item (Pnot (Stat a)) = ItemN a
pred2Item (Stat a)        = ItemP a
pred2Item a = error $ show a

filterDisjs :: Set Item -> Bool
filterDisjs disjs = not $ any
  (\a -> invItem a `elem` disjs)
  disjs

isCnfProved :: CNF -> Bool
isCnfProved (CNF set) = Set.empty `elem` set

invItem :: Item -> Item
invItem (ItemP a) = ItemN a
invItem (ItemN a) = ItemP a

predHasTrue :: Pred -> Bool
predHasTrue (Or a b) = predHasTrue a || predHasTrue b
predHasTrue Ptrue    = True
predHasTrue _        = False

disjSep :: String
disjSep = concat [s2, "|", s2]