module Lisp (
  s,
  v,
  ident,
  ident',
  list,
  uni,
  n,
  m,
  isNat,
  isInt,
  getNat,
  getInt,
  len,
  lenp,
  lenm,
  e,
  a,
  ta,
  empty,
  t,
  fst,
  snd,
  last,
  elems,
  chNum,
  getCh,
  err
) where

import Prelude hiding (fst, snd, last, break)

import Types
import Error

import Util hiding (isNat)
import qualified Util

type M = Either Error

s :: Node -> M Bool
s node = case getElem node of
  Ident _ -> Right True
  _       -> Right False

v :: Node -> M Bool
v node = case getElem node of
  List _ -> Right True
  _      -> Right False

ident :: Node -> M ()
ident node = case getElem node of
  Ident _ -> return ()
  _       -> err node $ exg "an identifier" "a list"

ident' :: Node -> String -> M ()
ident' node name = do
  ident node
  let Ident s = getElem node
  if s == name
    then return ()
    else err node $ exg (show name) (show s)

list :: Node -> M ()
list node = case getElem node of
  List _ -> return ()
  _      -> err node $ exg "a list" "an identifier"

uni :: Node -> M Node
uni node = do
  list node
  case getElem node of
    List (x:[]) -> return x
    List n      -> err node $ exg
      "exactly one element" $ if null n
        then "an empty list"
        else show (length n) ++ " elements"

n :: Node -> M Int
n node = do
  list node
  let (List xs) = getElem node
  return $ length xs

m :: Node -> M String
m node = do
  ident node
  let (Ident name) = getElem node
  return name

isNat :: Node -> M Bool
isNat node = do
  name <- m node
  return $ Util.isNat name

isInt :: Node -> M Bool
isInt node = do
  name <- m node
  case name of
    ('-':'0':_) -> return False
    ('-':xs)    -> return $ Util.isNat xs
    xs          -> return $ Util.isNat xs

getNat :: Node -> M Integer
getNat node = do
  nat <- isNat node
  if nat
    then do
      name <- m node
      return $ read $ name
    else err node $ ex "a natural number"

getInt :: Node -> M Integer
getInt node = do
  int <- isInt node
  if int
    then do
      name <- m node
      return $ read $ name
    else err node $ ex "an integer"

len :: Node -> Int -> Int -> M ()
len node x y = do
  z <- n node
  if z >= x && z <= y
    then return ()
    else err node $ concat [
      "The length of the list must be between ",
      show x, " and ", show y,
      " (inclusive), but it is ", show z]

lenp :: Node -> Int -> M ()
lenp node x = do
  z <- n node
  if z >= x
    then return ()
    else err node $ concat [
      "The length of the list must be at least ",
      show x, ", but it is ", show z]

lenm :: Node -> Int -> M ()
lenm node x = do
  z <- n node
  if z <= x
    then return ()
    else err node $ concat [
      "The length of the list must be at most ",
      show x, ", but it is ", show z]

e :: Node -> Int -> M Node
e node i = do
  lenp node (i + 1)
  xs <- elems node
  return $ xs !! i

a :: Node -> Int -> (Node -> M a) -> M [a]
a node i f = do
  lenp node i
  xs <- elems node
  mapM f $ drop i xs

ta :: Node -> String -> (Node -> M a) -> M [a]
ta node name f = do
  t node name
  xs <- elems node
  mapM f $ tail xs

empty :: Node -> M Bool
empty node = do
  a <- n node
  return $ a == 0

t :: Node -> String -> M ()
t node name = do
  a <- fst node
  ident' a name

fst :: Node -> M Node
fst node = e node 0

snd :: Node -> M Node
snd node = e node 1

last :: Node -> M Node
last node = do
  a <- n node
  e node (a - 1)

elems :: Node -> M [Node]
elems node = do
  list node
  let (List xs) = getElem node
  return xs

chNum :: Node -> M Int
chNum node = case getElem node of
  List xs -> return $ length xs
  _       -> return 0

getCh :: Node -> Int -> M Node
getCh = e

err :: Node -> String -> M a
err node msg = let
  file = getFile node
  pos = getPos node
  in Left $ Error {
    getErrFile = file,
    getErrPos = pos,
    getMsg = msg
  }