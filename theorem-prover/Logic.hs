{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Logic where

import Data.Kind

import qualified Prelude as P

infixl 9 .
type (.) = Call

type Bool a = (a -> a) -> a -> a

type F :: a -> Type
data F a where
  K    :: F (a -> b -> a)
  S    :: F ((a -> b -> c) -> (a -> b) -> a -> c)
  Call :: F (a -> b) -> F a -> F b
  Eq   :: F (a -> a -> Bool b)
  Nat  :: F (a -> Bool b)
  All  :: F (a -> Bool b -> Bool c)
  Imp  :: F (Bool a -> Bool b -> Bool c)
  The  :: F (a -> Bool b -> a)

type Id    = S . K . K
type Dot   = S . (K . S) . K
type Flip  = S . (Dot . Dot . S) . (K . K)
type Zero  = K . Id
type Suc   = S . Dot
type One   = Suc . Zero
type False = Zero
type True  = One

type P :: F a -> Type
data P a where
  PTrue :: P True
  Refl  :: P (All . (S . Eq . Id))