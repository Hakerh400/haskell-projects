module Expression
  ( Expr(..)
  , IdentType(..)
  , getIdentType
  , getVars
  , hasVar
  , substIdentE
  ) where

import qualified Data.Char as Char

import qualified Data.Set as Set
import Data.Set (Set)

data Expr =
  ExprI IdentType String |
  ExprP Expr Expr
  deriving (Eq, Ord)

data IdentType = Var | Const
  deriving (Eq, Ord, Show)

getIdentType :: String -> IdentType
getIdentType (x:xs) = if Char.isLower x && all Char.isAlphaNum xs
  then Var
  else Const

getVars :: Expr -> Set String
getVars (ExprI Const _) = Set.empty
getVars (ExprI Var   a) = Set.singleton a
getVars (ExprP a     b) = getVars a `Set.union` getVars b

hasVar :: String -> Expr -> Bool
hasVar name expr = name `elem` getVars expr

substIdentE :: String -> Expr -> Expr -> Expr
substIdentE x y (ExprI t a) = if a == x
  then y
  else ExprI t a
substIdentE x y (ExprP a b) = ExprP (substIdentE x y a) (substIdentE x y b)