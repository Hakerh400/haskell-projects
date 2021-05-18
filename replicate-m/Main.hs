{-# LANGUAGE RankNTypes #-}

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

func :: [a] -> Integer -> [[a]]
func list n = nest (list >>=) n

t a b c = a (b c)
p = flip

h = t

-- a (b c) ---> t a b c

nest :: (Monad m) => ((a -> m [a]) -> m [a]) -> Integer -> m [a]
nest f n = nest' f f f (t return) (p (:) []) (:) n

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
nest' f x x' y z z' n = case n of
  1 -> x (y z)
  n -> let
    x'' = t x'
    z'' = t t z'
    in nest' f
      {- x  -} (h x x'')
      {- x' -} x''
      {- y  -} (t y)
      {- z  -} (p z'' z)
      {- z' -} z''
      (n - 1)

test :: (Monad m) => ((a -> m [a]) -> m [a]) -> Integer -> m [a]
test f 1 = h f                                                      (t return)                                                               (p (:) [])
test f 2 = h (h f (t f))                                         (t (t return))                                                 (p (t t (:)) (p (:) []))
test f 3 = h (h (h f (t f)) (t (t f)))                        (t (t (t return)))                             (p (t t (t t (:))) (p (t t (:)) (p (:) [])))
test f 4 = h (h (h (h f (t f)) (t (t f))) (t (t (t f))))   (t (t (t (t return))))   (p (t t (t t (t t (:)))) (p (t t (t t (:))) (p (t t (:)) (p (:) []))))