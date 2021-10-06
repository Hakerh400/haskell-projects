{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

import Data.Kind

import qualified Prelude as P

import Logic

main :: P.IO ()
main = P.putStrLn "ok"

true :: True
true = Refl

nfalse :: Not False
nfalse a = a

tr1 :: forall a b c. a == b -> a == c -> c == b
tr1 ab ac = Tran (Com ac) ab

tr2 :: forall a b c. a == b -> b == c -> a == c
tr2 = Tran

tr1' :: forall a b c. a == b -> b == c -> a == c
tr1' = tr2

tr2' :: forall a b c. a == b -> c == b -> c == a
tr2' ab cb = Tran cb (Com ab)

infixr 0 `tr1'`
infixr 0 `tr2'`

conga :: forall f a b. a == b -> f . a == f . b
conga = Cong Refl

congf :: forall f g a. f == g -> f . a == g . a
congf fg = Cong fg Refl

congf2 :: forall f g a b. f == g -> f . a . b == g . a . b
congf2 fg = congf (congf fg)

id :: forall a. I . a == a
id = Refl `tr2` SDef `tr2` KDef

dot :: forall a b c. D . a . b . c == a . (b . c)
dot = let
  dot_a = Refl `tr2` SDef `tr2` congf KDef :: D . a == S . (K . a)
  in congf2 dot_a `tr2` SDef `tr2` congf KDef

flip :: forall a b c. F . a . b . c == a . c . b
flip = let
  flip_a = Refl `tr2` SDef `tr2` congf dot `tr2` conga KDef :: F . a == D . (S . a) . K
  flip_ab = congf flip_a `tr2` dot :: F . a . b == S . a . (K . b)
  in congf flip_ab `tr2` SDef `tr2` conga KDef

sk_a_id :: forall a. S . K . a == I
sk_a_id = let
  sk_ab = id `tr2'` SDef `tr1'` KDef :: forall a b. S . K . a . b == I . b
  in Ext sk_ab

type FI = F . I

flip_id :: forall a b. FI . a . b == b . a
flip_id = Refl `tr2` flip `tr2` congf id

type B0 = K . I
type B1 = K

b0 :: forall a b. B0 . a . b == b
b0 = Refl `tr2` congf KDef `tr2` id

b1 :: forall a b. B1 . a . b == a
b1 = KDef

b0_neq_b1 :: B0 /= B1
b0_neq_b1 b0_b1 = let
  b0s_b1s = congf b0_b1 :: B0 . S == B1 . S
  b0sk_b1sk = congf b0s_b1s :: B0 . S . K == B1 . S . K
  in b0sk_b1sk `tr1` b0 `tr2` b1

type Iota = F . (FI . S) . K

iota :: forall a. Iota . a == a . S . K
iota = flip `tr1'` congf flip_id

iota_b0_k :: Iota . B0 == K
iota_b0_k = iota `tr1'` b0

iota_b1_k :: Iota . B1 == S
iota_b1_k = iota `tr1'` b1

type NZ = F . (FI . (K . B1)) . B0

nz :: forall n. NZ . n == n . (K . B1) . B0
nz = flip `tr1'` congf flip_id

nz_0_b0 :: NZ . Zero == B0
nz_0_b0 = Refl `tr2` flip `tr2` congf flip_id `tr2` congf KDef `tr2` id

nz_suc_b1 :: forall n. NZ . (Suc . n) == B1
nz_suc_b1 = nz `tr1'` congf SDef `tr1'` dot `tr1'` KDef

zero_neq_suc :: forall n. Zero /= Suc . n
zero_neq_suc zero_suc = let
  b0_b1 = conga zero_suc `tr1` nz_0_b0 `tr2` nz_suc_b1 :: B0 == B1
  in b0_neq_b1 b0_b1

-- suc_inj :: forall n m. Nat n -> Nat m -> Suc . n == Suc . m -> n == m
-- suc_inj nNat mNat suc_eq = let
--   in _

-- u_ :: a
-- u_ = P.undefined