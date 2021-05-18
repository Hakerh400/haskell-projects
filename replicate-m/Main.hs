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

t = (.)
p = flip

-- a (b c) ---> t a b c

nest :: (Monad m) => ((a -> m [a]) -> m [a]) -> Integer -> m [a]
nest f 1 = t f                                                               (t return)                                                            (p (:) [])
nest f 2 = t f (t f)                                                      (t (t return)                                               (p (t t (:)) (p (:) [])))
nest f 3 = t (t f (t f)) (t (t f))                                     (t (t (t return))                           (p (t t (t t (:))) (p (t t (:)) (p (:) []))))
nest f 4 = t (t (t (t f (t f))) (t (t (t f)))) (t (t (t (t (t f)))) t) (t (t (t return))) (p (t t (t t (t t (:)))) (p (t t (t t (:))) (p (t t (:)) (p (:) []))))