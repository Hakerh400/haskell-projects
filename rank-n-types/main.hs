{-# LANGUAGE RankNTypes #-}

main :: IO ()
main = do
  print $ func (:[]) head 3 "abc"
  return ()

func :: (forall a. a -> [a]) -> (forall a. [a] -> a) -> Integer -> a -> a
func f g 0 a = a
func f g 1 a = g (f a)
func f g 2 a = g (g (f (f a)))
func f g 3 a = g (g (g (f (f (f a)))))