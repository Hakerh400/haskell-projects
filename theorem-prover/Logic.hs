module Logic
  ( Ctx
  , initCtx
  , apply
  
  , fstIdent
  , nextIdent
  , mkImp
  , mkNot
  , ax1
  , ax2
  , ax3
  ) where

import Data.Char

import Base

type Rule = Ctx -> Ctx

data Ctx = Ctx [Proof] [Expr]
  deriving (Eq)

data Proof = Proof Expr
   deriving (Eq)

data Expr
  = Ident Nat
  | Imp Expr Expr
  | Not Expr
  deriving (Eq)

data Nat
  = Zero
  | Suc Nat
  deriving (Eq)

instance Show Ctx where
  show (Ctx a b)
    = trimRight
    $ join "\n\n"
    $ map (join "\n") [map show a, map show b]

instance Show Proof where
  show (Proof a) = show a

instance Show Expr where
  show (Ident n) = [['A'..] !! nat2int n]
  show (Imp a b) = concat [expr2str a, " -> ", show b]
  show (Not a) = concat ["~", expr2str a]

expr2str :: Expr -> String
expr2str a = case a of
  Imp _ _ -> inParens s
  _ -> s
  where s = show a

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Suc n) = 1 + nat2int n

initCtx :: Ctx
initCtx = Ctx [] []

apply :: Ctx -> [Rule] -> Ctx
apply ctx [] = ctx
apply ctx (rule:rules) = apply (rule ctx) rules

fstIdent (Ctx a b) = Ctx a (Ident Zero : b)
nextIdent (Ctx a (Ident b : c)) = Ctx a (Ident (Suc b) : c)
mkImp (Ctx a (b : c : d)) = Ctx a (Imp c b : d)
mkNot (Ctx a (b : c)) = Ctx a (Not b : c)
ax1 (Ctx a (b : c : d)) = Ctx (Proof (Imp c (Imp b c)) : a) d
ax2 (Ctx a (b : c : d : e)) = Ctx (Proof (Imp (Imp d (Imp c b)) (Imp (Imp d c) (Imp d b))) : a) e
ax3 (Ctx a (b : c : d)) = Ctx (Proof (Imp (Imp (Not c) (Not b)) (Imp b c)) : a) d