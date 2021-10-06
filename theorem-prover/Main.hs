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

infixl 0 *>
(*>) :: forall a b c. a == b -> a == c -> c == b
(*>) ab ac = Tran (Com ac) ab

infixl 0 >*
(>*) :: forall a b c. a == b -> b == c -> a == c
(>*) = Tran

infixr 0 *<
(*<) :: forall a b c. a == b -> b == c -> a == c
(*<) = (>*)

infixr 0 <*
(<*) :: forall a b c. a == b -> c == b -> c == a
(<*) ab cb = Tran cb (Com ab)

conga :: forall f a b. a == b -> f . a == f . b
conga = Cong Refl

congf :: forall f g a. f == g -> f . a == g . a
congf fg = Cong fg Refl

congf2 :: forall f g a b. f == g -> f . a . b == g . a . b
congf2 fg = congf (congf fg)

id :: forall a. I . a == a
id = SDef >* KDef

dot :: forall a b c. D . a . b . c == a . (b . c)
dot = let
  dot_a = SDef >* congf KDef :: D . a == S . (K . a)
  in congf2 dot_a >* SDef >* congf KDef

flip :: forall a b c. F . a . b . c == a . c . b
flip = let
  flip_a = SDef >* congf dot >* conga KDef :: F . a == D . (S . a) . K
  flip_ab = congf flip_a >* dot :: F . a . b == S . a . (K . b)
  in congf flip_ab >* SDef >* conga KDef

sk_a_id :: forall a. S . K . a == I
sk_a_id = let
  sk_ab = id <* SDef *< KDef :: forall a b. S . K . a . b == I . b
  in Ext sk_ab

type FI = F . I

fi :: forall a b. FI . a . b == b . a
fi = flip >* congf id

type B0 = K . I
type B1 = K

b0 :: forall a b. B0 . a . b == b
b0 = congf KDef >* id

b1 :: forall a b. B1 . a . b == a
b1 = KDef

b0_neq_b1 :: B0 /= B1
b0_neq_b1 b0_b1 = let
  b0s_b1s = congf b0_b1 :: B0 . S == B1 . S
  b0sk_b1sk = congf b0s_b1s :: B0 . S . K == B1 . S . K
  in b0sk_b1sk *> b0 >* b1

type Iota = F . (FI . S) . K

iota :: forall a. Iota . a == a . S . K
iota = flip *< congf fi

iota_b0_k :: Iota . B0 == K
iota_b0_k = iota *< b0

iota_b1_k :: Iota . B1 == S
iota_b1_k = iota *< b1

type NZ = F . (FI . (K . B1)) . B0

nz :: forall n. NZ . n == n . (K . B1) . B0
nz = flip *< congf fi

nz_0_b0 :: NZ . Zero == B0
nz_0_b0 = flip >* congf fi >* congf KDef >* id

nz_suc_b1 :: forall n. NZ . (Suc . n) == B1
nz_suc_b1 = nz *< congf SDef *< dot *< KDef

zero_neq_suc :: forall n. Zero /= Suc . n
zero_neq_suc zero_suc = let
  b0_b1 = conga zero_suc *> nz_0_b0 >* nz_suc_b1 :: B0 == B1
  in b0_neq_b1 b0_b1

infixr 3 &&
type a && b = Not (a -> b -> False)

infixr 2 ||
type a || b = Not a -> Not b -> False

-- notE :: forall a. Not (Not a) -> a
-- notE nna = nna

-- nz_b0_imp_z :: forall n. Nat n -> NZ . n == B0 -> n == Zero
-- nz_b0_imp_z nNat nz_b0 = let
--   in _

type FI2 = D . F . FI

fi2 :: forall a b c. FI2 . a . b . c == c . a . b
fi2 = congf2 dot >* flip >* congf fi

-- type NatId = FI2 . Suc . Zero
-- 
-- nat_id :: forall n. Nat n -> NatId . n == n
-- nat_id nNat = let
--   in Induct _ _ nNat

-- suc_inj :: forall n m. Nat n -> Nat m -> Suc . n == Suc . m -> n == m
-- suc_inj nNat mNat suc_eq = let
--   in _

-- u_ :: a
-- u_ = P.undefined