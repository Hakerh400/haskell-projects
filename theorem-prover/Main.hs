{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

import Data.Kind

import qualified Prelude as P

import Logic

main :: P.IO ()
main = P.putStrLn "ok"

infix 4 ==
infix 4 /=

type (==) = Eq
type (/=) = Neq

-- kSimp :: forall a b. P (K . a . b == a)
-- kSimp = u_

u_ :: a
u_ = P.undefined