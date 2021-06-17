module ParserTypes (
  File(..),
  Pos(..),
  Node(..),
  Elem(..)
) where

import qualified Data.List as List

import Util

data File = File {
  getFname :: String,
  getFsrc  :: String
}

data Pos = Pos {
  getRow :: Int,
  getCol :: Int
}

data Node = Node {
  getFile :: File,
  getPos  :: Pos,
  getElem :: Elem
}

data Elem =
  Ident String |
  List  [Node]

instance Show Node where
  show = show . getElem

instance Show Elem where
  show (Ident name) = name
  show (List  list) = parens $ sp $ fmap show list