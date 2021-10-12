module Program
  ( main
  ) where

import qualified Prelude as P

import Unit
import Identity
import Pair
import Nat
import List
import String

main :: String -> String
main a = str_from_lit "Hello, World!"