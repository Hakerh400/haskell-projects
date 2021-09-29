module Serializer
  ( SerT(..)
  , Ser
  , nz
  , lt
  , write
  , write2
  , read
  , read2
  , inc
  , inc1
  , writeNat
  , writeNat'
  , readNat
  , readNat'
  , getTable
  , setTable
  , getOutput
  ) where

import Control.Monad.State

import Prelude hiding (read)

import Base
import Tree

type Elem = (N, N)
type Stack = [Elem]

data SerT = SerT
  { num   :: N
  , stack :: Stack
  , table :: [Tree]
  }

type Ser = State SerT

getNum :: Ser N
getNum = gets num

getStack :: Ser Stack
getStack = gets stack

setNum :: N -> Ser ()
setNum num = modify $ \s -> s {num = num}

setStack :: Stack -> Ser ()
setStack stack = modify $ \s -> s {stack = stack}

modifyNum :: (N -> N) -> Ser ()
modifyNum f = modify $ \s -> s {num = f $ num s}

modifyStack :: (Stack -> Stack) -> Ser ()
modifyStack f = modify $ \s -> s {stack = f $ stack s}

push :: Elem -> Ser ()
push e = modifyStack (e:)

nz :: Ser Bool
nz = do
  n <- getNum
  if n /= 0
    then do
      setNum $ n - 1
      return True
    else return False

lt :: N -> Ser (Maybe N)
lt k = do
  n <- getNum
  if n < k
    then return $ Just n
    else do
      setNum $ n - k
      return Nothing

write :: N -> N -> Ser ()
write m n = push (m, n)

write2 :: N -> Ser ()
write2 = write 2

read :: N -> Ser N
read m = do
  n <- getNum
  setNum $ n `div` m
  return $ n `mod` m

read2 :: Ser N
read2 = read 2

inc :: N -> Ser ()
inc = write 1

inc1 :: Ser ()
inc1 = inc 1

writeNat :: N -> Ser ()
writeNat n = f $ n + 1 where
  f 1 = write2 0
  f n = do
    write2 1
    write2 $ n `mod` 2
    f $ n `div` 2

writeNat' :: N -> Ser ()
writeNat' = inc

readNat :: Ser N
readNat = do
  n <- readNatAux
  return $ n - 1

readNatAux :: Ser N
readNatAux = do
  b <- read2
  if b == 0
    then return 1
    else do
      a <- read2
      b <- readNatAux
      return $ a + 2 * b

readNat' :: Ser N
readNat' = getNum

getTable :: Ser [Tree]
getTable = gets table

setTable :: [Tree] -> Ser ()
setTable ts = get >>= \s -> put s {table = ts}

getOutput :: Ser N
getOutput = do
  stack <- getStack
  return $ foldl iterStack 0 stack

iterStack :: N -> Elem -> N
iterStack r (m, n) = r * m + n