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
  Imp  :: F (Bool a -> Bool b -> Bool c)
  All  :: F ((a -> Bool b) -> Bool c)
  The  :: F ((a -> Bool b) -> a)

type Id    = S . K . K
type Dot   = S . (K . S) . K
type Flip  = S . (Dot . Dot . S) . (K . K)
type Zero  = K . Id
type Suc   = S . Dot
type One   = Suc . Zero
type False = Zero
type True  = One
type Not   = Flip . Imp . False
type Neq   = Dot . (Dot . Not) . Eq

type P :: F a -> Type
data P a where
  PTrue :: P True
  TFDif :: P (Neq . True . False)
  KDef  :: P (Eq . (K . a . b) . a)
  SDef  :: P (Eq . (S . a . b . c) . (a . c . (b . c)))
  AllI  :: (forall a. P (f . a)) -> P (All . f)
  AllE  :: P (All . f) -> P (f . a)
  Refl  :: P (All . (S . Eq . Id))
  Sub   :: P (All . (Dot . (S . (Dot . S . (Dot . Imp . Eq))) . (S . (Flip . (Dot . (Dot . Dot) . (Dot . Imp)) . Id))))
  Ext   :: P (All . (S . (Dot . S . (Dot . (Dot . Imp) . (Dot . (Dot . All) . (Dot . S . (Dot . Eq))))) . Eq))
  TheAx :: P (All . (S . (Dot . Eq . (Dot . The . Eq)) . Id))
