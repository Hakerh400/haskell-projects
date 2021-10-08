module Logic
  ( Nat
  , Sort
  , Context
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

data Context = MkContext
  { context_idents    :: Map String (Sort, Nat)
  , context_types     :: Map Nat TypeDef
  , context_vals      :: Map Nat ValDef
  , context_typeNames :: Map Nat String
  , context_valNames  :: Map Nat String
  } deriving (Eq, Show)

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

initCtx :: Context
initCtx = MkContext
  { context_idents    = Map.empty
  , context_types     = Map.empty
  , context_vals      = Map.empty
  , context_typeNames = Map.empty
  , context_valNames  = Map.empty
  }

defType :: String -> Nat -> Context -> Context
defType name arity ctx = assertNoDef name ctx $ ctx
  { context_idents = Map.insert
    name (SortType, index) $ context_idents ctx
  , context_types = Map.insert
    index typeDef $ context_types ctx
  , context_typeNames = Map.insert
    index name $ context_typeNames ctx
  } where
    index = Map.size $ context_types ctx
    typeDef = MkTypeDef {typeDef_arity = arity}

assertNoDef :: String -> Context -> a -> a
assertNoDef name ctx = if name `Map.member` context_idents ctx
  then err ["Identifier ", show name, " has already been defined"]
  else id