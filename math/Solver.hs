module Solver
  ( solve
  , makeEq
  ) where

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

import Util
import Expression
import CNF

type Equation = (Expr, Expr)

solve :: Set Equation -> Maybe (Map String Expr)
solve eqs = case fstElem eqs of
  Nothing -> return $ Map.empty
  Just eq -> do
    eqs <- return $ Set.delete eq eqs
    let (lhs, rhs) = eq

    case lhs of
      ExprP a b -> case rhs of
        ExprP c d -> do
          eqs <- return $ Set.insert (makeEq a c) eqs
          eqs <- return $ Set.insert (makeEq b d) eqs
          solve eqs
      
      ExprI Const a -> case rhs of
        ExprI Const b -> if b == a
          then solve eqs
          else Nothing
        ExprP _ _ -> Nothing
      
      ExprI Var a -> if hasVar a rhs
        then Nothing
        else do
          eqs <- return $ mapSet (mapPair $ substIdentE a rhs) eqs
          sol <- solve eqs
          return $ Map.insert a (substIdentsE sol rhs) sol

makeEq :: Expr -> Expr -> Equation
makeEq = sortPair