{-# LANGUAGE RankNTypes #-}

main :: IO ()
main = do
  print $ func (:[]) head 3 "abc"
  return ()

func :: (forall a. a -> [a]) -> (forall a. [a] -> a) -> Integer -> a -> a
func f g 0 a = a
func f g n a = func f g (n - 1) (g (f a))