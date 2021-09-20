f a b = g (h (length a) [0]) a b
g (x:xs) a b = let
  s1 = x >>= (a!!)
  s2 = x >>= (b!!)
  in if s1 == s2
    then (map (+1) x, s1)
    else g xs a b
h n x = mapM (pure [0..n-1]) x ++ h n (0:x)

main = do
  let (x, s) = f a b
  print x
  print s

a = ["", "", "da", "dc", "db"]
b = ["adbdc", "d", "", "", ""]