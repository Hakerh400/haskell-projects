{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = putStrLn output;

output :: String
output = snd $ func (<= 'b') (replicate 3 . last) "abc"

func :: forall a b. (a -> Bool) -> ([a] -> b) -> [a] -> ([a], b)
func f g s = (result, g result) where
  result :: [a]
  result = takeWhile f s