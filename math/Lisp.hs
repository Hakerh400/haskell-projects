module Lisp
  ( s
  , v
  , ident
  , ident'
  , list
  , uni
  , n
  , m
  , isNat
  , isInt
  , getNat
  , getInt
  , len
  , len'
  , lenp
  , lenm
  , e
  , a
  , ta
  , empty
  , nempty
  , t
  , t'
  , fst
  , snd
  , last
  , elems
  , elems'
  , chNum
  , getCh
  , err
  ) where

import Prelude hiding (fst, snd, last, break)

import ParserTypes
import Error
import MonadE

import Util hiding (isNat)
import qualified Util

s :: (MonadE m) => Node -> m Bool
s node = case getElem node of
  Ident _ -> return True
  _       -> return False

v :: (MonadE m) => Node -> m Bool
v node = case getElem node of
  List _ -> return True
  _      -> return False

ident :: (MonadE m) => Node -> m ()
ident node = case getElem node of
  Ident _ -> return ()
  _       -> err node $ exg "an identifier" "a list"

ident' :: (MonadE m) => Node -> String -> m ()
ident' node name = do
  ident node
  let Ident s = getElem node
  if s == name
    then return ()
    else err node $ exg (show name) (show s)

list :: (MonadE m) => Node -> m ()
list node = case getElem node of
  List _ -> return ()
  _      -> err node $ exg "a list" "an identifier"

uni :: (MonadE m) => Node -> m Node
uni node = do
  list node
  case getElem node of
    List (x:[]) -> return x
    List n      -> err node $ exg
      "exactly one element" $ if null n
        then "an empty list"
        else show (length n) ++ " elements"

n :: (MonadE m) => Node -> m Int
n node = do
  list node
  let (List xs) = getElem node
  return $ length xs

m :: (MonadE m) => Node -> m String
m node = do
  ident node
  let (Ident name) = getElem node
  return name

isNat :: (MonadE m) => Node -> m Bool
isNat node = do
  name <- m node
  return $ Util.isNat name

isInt :: (MonadE m) => Node -> m Bool
isInt node = do
  name <- m node
  case name of
    ('-':'0':_) -> return False
    ('-':xs)    -> return $ Util.isNat xs
    xs          -> return $ Util.isNat xs

getNat :: (MonadE m) => Node -> m Integer
getNat node = do
  nat <- isNat node
  if nat
    then do
      name <- m node
      return $ read $ name
    else err node $ ex "a natural number"

getInt :: (MonadE m) => Node -> m Integer
getInt node = do
  int <- isInt node
  if int
    then do
      name <- m node
      return $ read $ name
    else err node $ ex "an integer"

len :: (MonadE m) => Node -> Int -> m ()
len node x = do
  z <- n node
  if z == x
    then return ()
    else err node $ concat [
      "The length of this list must be ", show x,
      ", but it is ", show z]

len' :: (MonadE m) => Node -> Int -> Int -> m ()
len' node x y = do
  z <- n node
  if z >= x && z <= y
    then return ()
    else err node $ concat [
      "The length of this list must be between ",
      show x, " and ", show y,
      " (inclusive), but it is ", show z]

lenp :: (MonadE m) => Node -> Int -> m ()
lenp node x = do
  z <- n node
  if z >= x
    then return ()
    else err node $ concat [
      "The length of this list must be at least ",
      show x, ", but it is ", show z]

lenm :: (MonadE m) => Node -> Int -> m ()
lenm node x = do
  z <- n node
  if z <= x
    then return ()
    else err node $ concat [
      "The length of this list must be at most ",
      show x, ", but it is ", show z]

e :: (MonadE m) => Node -> Int -> m Node
e node i = do
  lenp node (i + 1)
  xs <- elems node
  return $ xs !! i

a :: (MonadE m) => Node -> Int -> (Node -> m a) -> m [a]
a node i f = do
  lenp node i
  xs <- elems node
  mapM f $ drop i xs

ta :: (MonadE m) => Node -> String -> (Node -> m a) -> m [a]
ta node name f = do
  t' node name
  xs <- elems node
  mapM f $ tail xs

empty :: (MonadE m) => Node -> m Bool
empty node = do
  a <- n node
  return $ a == 0

nempty :: (MonadE m) => Node -> m Bool
nempty node = do
  a <- n node
  return $ a /= 0

t :: (MonadE m) => Node -> m String
t node = fst node >>= m

t' :: (MonadE m) => Node -> String -> m ()
t' node name = do
  a <- fst node
  ident' a name

fst :: (MonadE m) => Node -> m Node
fst node = e node 0

snd :: (MonadE m) => Node -> m Node
snd node = e node 1

last :: (MonadE m) => Node -> m Node
last node = do
  lenp node 1
  a <- n node
  e node (a - 1)

elems :: (MonadE m) => Node -> m [Node]
elems node = do
  list node
  let (List xs) = getElem node
  return xs

elems' :: (MonadE m) => Node -> Int -> m [Node]
elems' node i = do
  lenp node i
  xs <- elems node
  return $ drop i xs

chNum :: (MonadE m) => Node -> m Int
chNum node = case getElem node of
  List xs -> return $ length xs
  _       -> return 0

getCh :: (MonadE m) => Node -> Int -> m Node
getCh = e

err :: (MonadE m) => Node -> String -> m a
err node msg = let
  file = getFile node
  pos = getPos node
  in throw $ Error {
    getErrFile = file,
    getErrPos = pos,
    getMsg = msg
  }