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

nest :: (Monad m) => ((a -> m [a]) -> m [a]) -> Integer -> m [a]
nest f n = nest' f f f (return .) (: []) (:) n

nest' :: (Monad m) =>
  ((a -> m [a]) -> m [a]) ->
  ((a -> b) -> m [a]) ->
  ((a -> b) -> b) ->
  ((a -> c) -> a -> b) ->
  (a -> c) ->
  (a -> c -> c) ->
  Integer ->
  m [a]
nest' f x1 x1' y1 z1 z1' 1 = x1 (y1 z1)
nest' f x1 x1' y1 z1 z1' n = let
  x2  = x1 . x2'
  x2' = (x1' .)
  y2  = (y1 .)
  z2  = flip z2' z1
  z2' = (.) . z1'
  in nest' f x2 x2' y2 z2 z2' (n - 1)