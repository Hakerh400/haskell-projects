module Predicate
  ( Pred(..)
  , builtinPredNames
  , isBuiltinPred
  , substIdentP
  , substQuantifier
  , getAvailConst
  , getAvailConst'
  , getAvailVar
  , getAvailVar'
  ) where

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe

import qualified Data.Set as Set
import Data.Set (Set)

import Util
import Expression
import State
import Avail

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

instance Show Pred where
  show (Forall a b) = concat ["V", a, " ", show b]
  show (Exists a b) = concat ["E", a, " ", show b]
  show (Impl   a b) = op "->" a b
  show (Equiv  a b) = op "<->" a b
  show (Or     a b) = op "|" a b
  show (And    a b) = op "&" a b
  show (Pnot   a  ) = "~" ++ show a
  show (Stat   a  ) = expr2str True a
  show (Ptrue     ) = "True"
  show (Pfalse    ) = "False"

instance Show Expr where
  show = expr2str True

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
  (ExprI _ a)   -> a
  (ExprP a b) -> let
    a1 = expr2str False a
    b1 = expr2str True  b
    c  = sp [a1, b1]
    in if ps
      then parens c
      else c

op :: String -> Pred -> Pred -> String
op a b c = parens $ sp [show b, a, show c]

substIdentP :: String -> Expr -> Pred -> Pred
substIdentP x y (Forall a b) = Forall a $ substQuantifier x y a b
substIdentP x y (Exists a b) = Exists a $ substQuantifier x y a b
substIdentP x y (Or     a b) = substIdentP x y a `Or`  substIdentP x y b
substIdentP x y (And    a b) = substIdentP x y a `And` substIdentP x y b
substIdentP x y (Pnot   a  ) = Pnot $ substIdentP x y a
substIdentP x y (Stat   a  ) = Stat $ substIdentE x y a
substIdentP x y a = error $ show a

substQuantifier :: String -> Expr -> String -> Pred -> Pred
substQuantifier x y a p = if a == x
  then p
  else substIdentP x y p

-- Const

constChars :: String
constChars = ['A'..'Z']

getAvailConst :: (Foldable t) => t String -> String
getAvailConst = fst $ createAvailIdentFuncs constChars

getAvailConst' :: (Foldable t) => t String -> String -> String
getAvailConst' = snd $ createAvailIdentFuncs constChars

-- Var

varChars :: String
varChars = filter (not . (`elem` "ol")) ['a'..'z']

getAvailVar :: (Foldable t) => t String -> String
getAvailVar = fst $ createAvailIdentFuncs varChars

getAvailVar' :: (Foldable t) => t String -> String -> String
getAvailVar' = snd $ createAvailIdentFuncs varChars

-- Avail identifier functions

createAvailIdentFuncs :: (Foldable t, Eq a) => [a] ->
  ( t [a] -> [a]
  , t [a] -> [a] -> [a])
createAvailIdentFuncs elems = (getAvailVar, getAvailVar') where
  availVar = Avail
    { availFirst = firstVar
    , availNext  = nextVar
    }

  getAvailVar = getAvail availVar
  getAvailVar' = getAvail' availVar

  firstElem = head elems
  lastElem = last elems

  firstVar = [firstElem]
  nextVar = reverse . nextVar' . reverse

  nextVar' []     = firstVar
  nextVar' (c:cs) = if c == lastElem
    then firstElem : nextVar' cs
    else nextVarChar c : cs

  nextVarChar a = getElem $ getIndex a + 1
  getElem a = elems !! a
  getIndex a = fromJust $ List.elemIndex a elems