{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Examples.Codidact.P279147.Test
  ( test
  ) where

import qualified Prelude as P

import PolyList

import Unit
import Identity
import Pair
import Nat
import List
import String

test :: [PolyList (String ': '[])]
test =
  [ str_lit "Hello, World!" # PNil
  ]