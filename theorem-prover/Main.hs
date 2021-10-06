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

u_ :: a
u_ = P.undefined