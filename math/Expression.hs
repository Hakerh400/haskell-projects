module Expression
  ( Expr(..)
  , IdentType(..)
  , getIdentType
  , getVars
  , hasVar
  , substIdentE
  , substIdentsE
  , substZippedExpr
  ) where

import qualified Data.Char as Char

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

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

substIdentsE :: Map String Expr -> Expr -> Expr
substIdentsE m (ExprI t a) = case Map.lookup a m of
  Nothing   -> ExprI t a
  Just expr -> expr
substIdentsE m (ExprP a b) = ExprP (substIdentsE m a) (substIdentsE m b)

substZippedExpr :: (String, String) -> Expr -> Expr
substZippedExpr (name1, name2) expr = substIdentE name1 (ExprI Var name2) expr