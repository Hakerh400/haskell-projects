main :: IO ()
main = print $ fac 5

fac :: Int -> Int
fac = length . f . flip replicate (.) where
  f a = last $ [id] : map (const $ a >> f (tail a)) a