{-# LANGUAGE RankNTypes #-}

import Data.List
import Control.Monad

main :: IO ()
main = do
  putStrLn $ intercalate "\n" $ func "01" 4
  return ()

func :: [a] -> Integer -> [[a]]
func list n = nest (list >>=) n

nest :: (Monad m) => ((a -> m [a]) -> m [a]) -> Integer -> m [a]
nest f 1 = f (\a -> return [a])
nest f 2 = f (\a -> f (\b -> return [a, b]))
nest f 3 = f (\a -> f (\b -> f (\c -> return [a, b, c])))
nest f 4 = f (\a -> f (\b -> f (\c -> f (\d -> return [a, b, c, d]))))

-- nest :: ((Char -> [[Char]]) -> [[Char]]) -> Integer -> [[Char]]
-- nest f 1 = f (t                                     return                                                             (p (:) []))
-- nest f 2 = f (t f (t                             (t return)                                               (p (t t (:)) (p (:) []))))
-- nest f 3 = f (t f (t (t f)                 (t (t (t return))                           (p (t t (t t (:))) (p (t t (:)) (p (:) []))))))
-- nest f 4 = f (t f (t (t f) (t (t (t f)) (t (t (t (t return))) (p (t t (t t (t t (:)))) (p (t t (t t (:))) (p (t t (:)) (p (:) []))))))))