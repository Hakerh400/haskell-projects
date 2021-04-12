main :: IO ()
main = putStrLn output;

output :: String
output = show $ split func "123 45"

func :: String -> String -> Maybe Integer
func a b = case b of
  [] -> Nothing
  (x:xs) -> case x of
    ' ' -> Just 1
    _ -> Nothing

split :: ([a] -> [a] -> Maybe Integer) -> [a] -> [[a]]
split func list = split' func 0 [] list [] []

split' :: ([a] -> [a] -> Maybe Integer) -> Integer -> [a] -> [a] -> [a] -> [[a]] -> [[a]]
split' func skip prev list sub acc = case list of
  [] -> case func prev list of
    Nothing -> reverse (reverse sub : acc)
    _ -> reverse ([] : reverse sub : acc)
  (x:xs) -> case skip of
    0 -> case func prev list of
      Nothing -> split' func 0 (x : prev) xs (x : sub) acc
      Just len -> case len of
        0 -> split' func 0 (x : prev) xs (x : []) (reverse sub : acc)
        n -> split' func (n - 1) (x : prev) xs [] (reverse sub : acc)
    n -> split' func (n - 1) (x : prev) xs sub acc