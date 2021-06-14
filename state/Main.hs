import Control.Applicative

newtype State s a = State
  {getState :: (s -> (a, s))}

instance Functor (State s) where
  fmap f (State st) = State $ \s ->
    let (a, s1) = st s
    in  (f a, s1)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  liftA2 f (State st1) (State st2) = State $ \s ->
    let (a1, s1) = st1 s
        (a2, s2) = st2 s1
    in  (f a1 a2, s2)

instance Monad (State s) where
  return = pure
  (State st1) >>= f = State $ \s ->
    let (a, s1) = st1 s
    in  getState (f a) s1

main :: IO ()
main = print output

output :: [(Int, Int)]
output = evalState (func [1..10]) 100

func :: [Int] -> State Int [(Int, Int)]
func [] = return []
func (x:xs) = do
  n <- get
  put $ n - 1
  xs' <- func xs
  return $ (x, n) : xs'

evalState :: State s a -> s -> a
evalState (State st) s = fst $ st s

execState :: State s a -> s -> s
execState (State st) s = snd $ st s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)