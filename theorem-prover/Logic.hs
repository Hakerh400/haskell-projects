module Logic
  ( Nat
  , Sort
  , Ctx
  , InCtx
  , TypeDef
  , ValDef
  , Expr
  , TypeExpr
  , ValExpr
  
  , initCtx
  , defType
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Base

type Nat = Int

data Sort
  = SortType
  | SortVal
  deriving (Eq, Show)

data Ctx = MkCtx
  { context_types :: [TypeDef]
  , context_vals  :: [ValDef]
  } deriving (Eq, Show)

type InCtx a = (Ctx, a)

data TypeDef = MkTypeDef
  { typeDef_arity :: Nat
  } deriving (Eq, Show)

data ValDef = MkValDef
  { valDef_expr :: Expr
  } deriving (Eq, Show)

data Expr = MkExpr
  { expr_typesN :: Nat
  , expr_valsN  :: Nat
  , expr_types  :: Set Nat
  , expr_vals   :: Set Nat
  , expr_type   :: TypeExpr
  , expr_val    :: ValExpr
  } deriving (Eq, Show)

data TypeExpr
  = GlobType Nat
  | LocType Nat
  | CompType TypeExpr TypeExpr
  deriving (Eq, Show)

data ValExpr
  = GlobVal Nat
  | LocVal Nat
  | CompVal Expr Expr
  deriving (Eq, Show)

initCtx :: Ctx
initCtx = MkCtx
  { context_types = []
  , context_vals  = []
  }

defType :: Nat -> Ctx -> Ctx
defType arity ctx = ctx
  { context_types = typeDef : context_types ctx
  } where
    typeDef = MkTypeDef {typeDef_arity = arity}