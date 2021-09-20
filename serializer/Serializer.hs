module Serializer
  ( N
  , Serializer
  , Ser
  , ser
  , deser
  , ser_init
  , nz
  , write
  , write2
  , read
  , read2
  , inc
  , inc1
  , get_output
  ) where

import Control.Monad.State

import Prelude hiding (read)

type N = Integer
type Elem = (N, N)
type Stack = [Elem]

data Serializer = Serializer
  { num :: N
  , stack :: Stack
  }

type Ser = State Serializer

get_num :: Ser N
get_num = gets num

get_stack :: Ser Stack
get_stack = gets stack

set_num :: N -> Ser ()
set_num num = modify $ \s -> s {num = num}

set_stack :: Stack -> Ser ()
set_stack stack = modify $ \s -> s {stack = stack}

modify_num :: (N -> N) -> Ser ()
modify_num f = modify $ \s -> s {num = f $ num s}

modify_stack :: (Stack -> Stack) -> Ser ()
modify_stack f = modify $ \s -> s {stack = f $ stack s}

push :: Elem -> Ser ()
push e = modify_stack (e:)

ser :: Ser () -> N
ser f = evalState (f >> get_output) $ ser_init 0

deser :: Ser a -> N -> a
deser f n = evalState f $ ser_init n

ser_init :: N -> Serializer
ser_init n = Serializer
  { num = n
  , stack = []
  }

nz :: Ser Bool
nz = do
  n <- get_num
  if n /= 0
    then do
      set_num $ n - 1
      return True
    else return False

write :: N -> N -> Ser ()
write m n = push (m, n)

write2 :: N -> Ser ()
write2 = write 2

read :: N -> Ser N
read m = do
  n <- get_num
  set_num $ n `div` m
  return $ n `mod` m

read2 :: Ser N
read2 = read 2

inc :: N -> Ser ()
inc = write 1

inc1 :: Ser ()
inc1 = inc 1

get_output :: Ser N
get_output = do
  stack <- get_stack
  return $ foldl iter_stack 0 stack

iter_stack :: N -> Elem -> N
iter_stack r (m, n) = r * m + n