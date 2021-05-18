{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = do
  print $ func (:[]) head () 3
  return ()

func :: (forall b. b -> m b) -> (forall c. m c -> c) -> a -> Integer -> a
func f g z n = func' f g z id id n

func' :: (forall b. b -> m b) -> (forall c. m c -> c) -> a -> (a -> d) -> (d -> a) -> Integer -> a
func' f g z p q 0 = q (p z)
func' f g z p q n = func' f g z (f . p) (q . g) (n - 1)