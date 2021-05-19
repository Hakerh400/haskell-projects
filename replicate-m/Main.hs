{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import Data.List
import Control.Monad

main :: IO ()
main = do
  printBlock 4
  return ()

printBlock :: Integer -> IO ()
printBlock n = printBlock' n 1

printBlock' :: Integer -> Integer -> IO ()
printBlock' n i = if i <= n
  then do
    putStrLn $ intercalate "\n" $ func "01" i
    
    if i /= n
      then putStrLn ""
      else return ()
    
    printBlock' n (i + 1)
  else return ()

func :: (a ~ Char) => [a] -> Integer -> [[a]]
func list n = nest (list >>=) n

t a b c = a (b c)
p = flip

h = t

-- a (b c) ---> t a b c

-- nest :: (Monad m) => ((a -> m [a]) -> m [a]) -> Integer -> m [a]
-- nest f n = nest' f f f (t return) (p (:) []) (:) n

-- nest' :: (Monad m) =>
--   ((a -> m [a]) -> m [a]) ->
--
--   -- x
--   (b -> m [a]) ->
--
--   -- x'
--   () ->
--
--   -- y
--   (c -> b) ->
--
--   -- z
--   (c) ->
--
--   -- z'
--   () ->
--
--   Integer ->
--   m [a]
-- nest' f x x' y z z' n = case n of
--   1 -> x (y z)
--   n -> let
--     x'' = t x'
--     z'' = t t z'
--     in nest' f
--       {- x  -} (h x x'')
--       {- x' -} x''
--       {- y  -} (t y)
--       {- z  -} (p z'' z)
--       {- z' -} z''
--       (n - 1)

nest = test

-- t                 :: (b -> c) -> (a -> b) -> a -> c
-- t (x -> y)        :: (a -> x) -> a -> y

-- t t               :: (d -> b -> c) -> d -> (a -> b) -> a -> c
-- t t (x -> y -> z) :: x -> (a -> y) -> a -> z

type C = Char
type S = [C]
type SS = [S]

type T31 = C -> C -> S
type T32 = C -> C -> SS

type T41 = C -> T31
type T42 = C -> T32

test :: forall a m. (Monad m, m ~ [], a ~ Char) => ((a -> m [a]) -> m [a]) -> Integer -> m [a]
test f n = test' f f f (t return) (p (:) []) (:) n
-- test f n = case n of
--     1 -> x1 $ y1 z1
--     2 -> x2 $ y2 z2
--     3 -> x3 $ y3 z3
--     4 -> x4 $ y4 z4
--   where
--     x1   = f         :: (a -> SS) -> SS
--     x1'  = f         :: (a -> SS) -> SS
--     y1   = t return  :: (a -> S) -> a -> SS
--     z1   = p (:) []  :: a -> S
--     z1'  = (:)       :: a -> S -> S

--     x2   = h x1 x2'  :: (a -> a -> SS) -> SS
--     x2'  = t x1'     :: (a -> a -> SS) -> a -> SS
--     y2   = t y1      :: (a -> a -> S) -> a -> a -> SS
--     z2   = p z2' z1  :: a -> a -> S
--     z2'  = t t z1'   :: a -> (a -> S) -> a -> S

--     x3   = h x2 x3'  :: (a -> T32) -> SS
--     x3'  = t x2'     :: (a -> T32) -> T32
--     y3   = t y2      :: (a -> T31) -> a -> T32
--     z3   = p z3' z2  :: a -> T31
--     z3'  = t t z2'   :: a -> T31 -> T31

--     x4   = h x3 x4'  :: (a -> T42) -> SS
--     x4'  = t x3'     :: (a -> T42) -> T42
--     y4   = t y3      :: (a -> T41) -> a -> T42
--     z4   = p z4' z3  :: a -> T41
--     z4'  = t t z3'   :: a -> T41 -> T41

test' :: (Monad m, m ~ [], a ~ C) =>
  ((a -> SS) -> SS) ->
  ((a -> t2) -> SS) ->
  ((a -> t2) -> t2) ->
  ((a -> t1) -> a -> t2) ->
  (a -> t1) ->
  (a -> t1 -> t1) ->
  Integer ->
  SS
test' f x1 x1' y1 z1 z1' 1 = x1 $ y1 z1
test' f x1 x1' y1 z1 z1' n = let
  x2   = h x1 x2'
  x2'  = t x1'
  y2   = t y1
  z2   = p z2' z1
  z2'  = t t z1'
  in test' f x2 x2' y2 z2 z2' (n - 1)

-- 1 -> x1 $ y1 z1
-- 2 -> (h f (t f))                                 $       (t (t return))                                                 (p (t t (:)) (p (:) []))
-- 3 -> (h (h f (t f)) (t (t f)))                   $    (t (t (t return)))                             (p (t t (t t (:))) (p (t t (:)) (p (:) [])))
-- 4 -> (h (h (h f (t f)) (t (t f))) (t (t (t f)))) $ (t (t (t (t return))))   (p (t t (t t (t t (:)))) (p (t t (t t (:))) (p (t t (:)) (p (:) []))))