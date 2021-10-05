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
  Call :: !Comb -> !Comb -> Comb

infixl 9 .
type (.) = Call

type I    = S . K . K
type D    = S . (K . S) . K
type F    = S . (D . D . S) . (K . K)
type Zero = K . (I)
type Suc  = S . D

infix 4 ==
type (==) = Eq
type Eq :: Comb -> Comb -> Type
data Eq a b where
  Refl    :: forall a. a == a
  Com     :: forall a b. !(a == b) -> b == a
  Tran    :: forall a b c. !(a == b) -> !(b == c) -> a == c
  Cong    :: forall f g a b. !(f == g) -> !(a == b) -> f . a == g . b
  Ext     :: forall f g. !(forall a. f . a == g . a) -> f == g
  KDef    :: forall a b. (K . a) . b == a
  SDef    :: forall a b c. ((S . a) . b) . c == a . c . (b . c)
  Induct  :: !(f Zero == r)
          -> !(forall m. Nat m -> f m == r -> f (Suc . m) == r)
          -> !(Nat n) -> f n == r

type Nat :: Comb -> Type
data Nat a where
  NatZero :: Nat Zero
  NatSuc  :: !(Nat n) -> Nat (Suc . n)