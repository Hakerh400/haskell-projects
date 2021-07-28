n = 5

data Jar =
  Empty |
  Pair Jar Jar

instance Show Jar where
  show Empty = "0"
  show (Pair a b) = concat ["(", show a, ", ", show b, ")"]

main :: IO ()
main = do
  let jar = r2 n Empty

  print jar
  print $ depth jar

r2 :: Int -> Jar -> Jar
r2 1 x = Pair Empty x
r2 n x = r2 (n - 1) (Pair (r1 n) x)

r1 :: Int -> Jar
r1 n = r2 (n - 1) Empty

depth :: Jar -> Int
depth Empty = 0
depth (Pair x y) = max (depth x + 1) (depth y)