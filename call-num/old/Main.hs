main :: IO ()
main = putStrLn output

output :: String
output = 0 "abc"

class ClassString a where
  fromString :: String -> a
  toString :: a -> String

class ClassChar a where
  fromChar :: Char -> a
  toChar :: a -> Char

instance (ClassChar a) => ClassString [a] where
  fromString = map fromChar
  toString = map toChar

instance ClassChar Char where
  fromChar = id
  toChar = id

instance (ClassString a, ClassString b) => Num (a -> b) where
  _ + _ = 0
  _ * _ = 0
  abs _ = 0
  signum _ = 0
  negate _ = 0
  fromInteger _ a = result where
    str = toString a
    result = fromString str