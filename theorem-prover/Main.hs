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

tr1 :: forall x y a. x == a -> x == y -> y == a
tr1 xa xy = Tran (Com xy) xa

tr2 :: forall x y a. a == x -> x == y -> a == y
tr2 ax xy = Tran ax xy

skk_a_eq_a :: forall a. S . K . K . a == a
skk_a_eq_a = SDef `tr2` KDef

-- id :: forall a. (I).a == a
-- id = u

u :: a
u = P.undefined