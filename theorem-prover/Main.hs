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

tr1 :: forall a b c. a == b -> a == c -> c == b
tr1 ab ac = Tran (Com ac) ab

tr2 :: forall a b c. a == b -> b == c -> a == c
tr2 = Tran

acong :: forall f a b. a == b -> f . a == f . b
acong = Cong Refl

fcong :: forall f g a. f == g -> f . a == g . a
fcong fg = Cong fg Refl

fcong2 :: forall f g a b. f == g -> f . a . b == g . a . b
fcong2 fg = fcong (fcong fg)

id :: forall a. I . a == a
id = Refl `tr2` SDef `tr2` KDef

dot :: forall a b c. D . a . b . c == a . (b . c)
dot = let
  dot_a = Refl `tr2` SDef `tr2` fcong KDef :: D . a == S . (K . a)
  in fcong2 dot_a `tr2` SDef `tr2` fcong KDef

flip :: forall a b c. F . a . b . c == a . c . b
flip = let
  flip_a = Refl `tr2` SDef `tr2` fcong dot `tr2` acong KDef :: F . a == D . (S . a) . K
  flip_ab = fcong flip_a `tr2` dot :: F . a . b == S . a . (K . b)
  in fcong flip_ab `tr2` SDef `tr2` acong KDef

type FI = F . I

flip_id :: forall a b. FI . a . b == b . a
flip_id = Refl `tr2` flip `tr2` fcong id