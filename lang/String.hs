module String
  ( String
  , str_from_lit
  ) where

import qualified Data.Char
import qualified Prelude as P

import Nat
import List
import Char

type String = List Char

str_from_lit :: P.String -> String
str_from_lit = P.foldr ((P..) cons char_from_hs) nil

char_from_hs :: P.Char -> Char
char_from_hs = (P..) nat_from_lit Data.Char.ord