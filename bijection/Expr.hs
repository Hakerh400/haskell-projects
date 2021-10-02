module Expr
  ( Expr(..)
  , callExpr
  , callFunc
  , exprToNat
  , exprToTree
  , exprToFunc
  , combArgsToExpr
  , mkPair
  , simpleTreeTable
  , Comb(..)
  , allCombs
  , combsNum
  , combArities
  , combsTable
  , applyComb
  ) where

import Base
import Tree
import TreeSer
import ProgInfo

-- Expr

data Expr
  = ExprNat N
  | ExprTree Tree
  | ExprFunc Comb [Expr]
  deriving (Eq, Show)

callExpr :: ProgInfo -> N -> Expr -> Expr -> Expr
callExpr info depth target arg =
  if depth == maxRecDepth info
    then ExprTree $ mkPair target arg
    else callFunc info (depth + 1) (exprToFunc target) arg

callFunc :: ProgInfo -> N -> (Comb, [Expr]) -> Expr -> Expr
callFunc info depth (comb, args) arg =
  applyComb info depth comb $ args ++ [arg]

exprToNat :: Expr -> N
exprToNat (ExprNat n) = n
exprToNat (ExprTree tree) =
  ser simpleTreeTable tree
exprToNat expr@(ExprFunc comb args) = let
  ctree = exprToCtree expr
  in ser combsTable ctree

exprToFunc :: Expr -> (Comb, [Expr])
exprToFunc (ExprFunc comb args) = (comb, args)
exprToFunc expr = let
  n = exprToNat expr
  ctree = deser combsTable n
  in ctreeToCombArgs ctree

exprToTree :: Expr -> Tree
exprToTree (ExprTree tree) = tree
exprToTree expr = let
  n = exprToNat expr
  in deser simpleTreeTable n

ctreeToCombArgs :: Tree -> (Comb, [Expr])
ctreeToCombArgs leaf@(Leaf _) = (leafToComb leaf, [])
ctreeToCombArgs (Node target arg) = let
  (comb, args) = ctreeToCombArgs target
  in (comb, args ++ [ctreeToExpr arg])

combArgsToExpr :: (Comb, [Expr]) -> Expr
combArgsToExpr = uncurry ExprFunc

ctreeToExpr :: Tree -> Expr
ctreeToExpr = combArgsToExpr . ctreeToCombArgs

exprToCtree :: Expr -> Tree
exprToCtree (ExprFunc comb args) =
  foldl f (combToLeaf comb) args where
    f ctree arg = Node ctree $ exprToCtree arg
exprToCtree expr = exprToCtree $ combArgsToExpr $ exprToFunc expr

mkPair :: Expr -> Expr -> Tree
mkPair a b = Node (exprToTree a) (exprToTree b)

simpleTreeTable :: Table
simpleTreeTable = [(Leaf 0, Nothing)]

-- Comb

data Comb
  = K
  | S
  | PAIR
  | EXA
  deriving (Bounded, Enum, Ord, Eq, Show)

allCombs :: [Comb]
allCombs = [minBound..maxBound]

combsNum :: N
combsNum = len allCombs

combArities :: [N]
combArities = [2, 3, 2, 3]

combsTable :: Table
combsTable = zipWith combTable' [0..combsNum-1] combArities where
  combTable' c n = (Leaf c, Just (n - 1))

applyComb :: ProgInfo -> N -> Comb -> [Expr] -> Expr
applyComb info depth comb args = case (comb, args) of
  (K, [a, b]) -> a
  (S, [a, b, c]) -> call (call a c) (call b c)
  (PAIR, [a, b]) -> ExprTree $ mkPair a b
  (EXA, [a, b, c]) -> undefined
  _ -> ExprFunc comb args
  where
    call = callExpr info depth

combToLeaf :: Comb -> Tree
combToLeaf comb = Leaf $ fromIntegral $ fromEnum comb

leafToComb :: Tree -> Comb
leafToComb (Leaf n) = toEnum $ fromIntegral n