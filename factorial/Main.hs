main :: IO ()
main = print $ fac 5

fac :: Int -> Int
fac = length . f . flip replicate (.) where
  f a = last $ (a >> f (tail a)) : [[id] | null a]