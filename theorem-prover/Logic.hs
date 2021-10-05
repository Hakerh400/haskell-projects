{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Logic where

import Data.Kind

import qualified Prelude as P

type Comb :: Type
data Comb where
  K    :: Comb
  S    :: Comb
  I    :: Comb
  D    :: Comb
  F    :: Comb
  Zero :: Comb
  Suc  :: Comb
  Call :: !Comb -> !Comb -> Comb

infixl 9 .
type (.) = Call

infix 4 ==
type (==) = Eq
type Eq :: Comb -> Comb -> Type
data Eq a b where
  Refl     :: forall a. a == a
  Com      :: forall a b. !(a == b) -> b == a
  Tran     :: forall a b c. !(a == b) -> !(b == c) -> a == c
  Sub      :: forall a b f. !(a == b) -> f . a == f . b
  Ext      :: forall f g. !(forall a. f . a == g . a) -> f == g
  KDef     :: forall a b. (K . a) . b == a
  SDef     :: forall a b c. ((S . a) . b) . c == a . c . (b . c)
  IDef     :: I == S . K . K
  DDef     :: D == S . (K . S) . K
  FDef     :: F == S . (D . D . S) . (K . K)
  ZeroDef  :: Zero == K . (I)
  SucDef   :: Suc == S . D
  Induct   :: !(f Zero == r)
           -> !(forall m. Nat m -> f m == r -> f (Suc . m) == r)
           -> !(Nat n) -> f n == r

type Nat :: Comb -> Type
data Nat a where
  NatZero :: Nat Zero
  NatSuc  :: !(Nat n) -> Nat (Suc . n)