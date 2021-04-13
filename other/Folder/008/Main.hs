{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = putStrLn output

data Lam =
  Ide String |
  Abs String Lam |
  App Lam Lam

output :: String
output = show $ tokenize func "a b -> (a b) (b a)"

func :: String -> String -> Char -> Bool
func prev str = (== head str)

-- str2lam :: String -> Lam
-- str2lam s = parse (tokenizeN s) where

lam2str :: Lam -> String
lam2str x = case x of
  Ide name -> name
  Abs arg expr -> "(" ++ arg ++ " -> " ++ (lam2str expr) ++ ")"
  App target arg -> (lam2str target) ++ " " ++ (lam2str arg)

tokenize :: forall a. ([a] -> [a] -> a -> Bool) -> [a] -> [[a]]
tokenize func s = tokenize' [] s [] where
  tokenize' prev str acc = case str of
    [] -> reverse acc
    _ -> tokenize' (token ++ prev) (dropF f str) (token : acc) where
      f :: a -> Bool
      f = func prev str
      token :: [a]
      token = takeF f str

takeF :: (a -> Bool) -> [a] -> [a]
takeF f s = case s of
  [] -> []
  (x:xs) -> case f x of
    True -> x : takeF f xs
    False -> []

dropF :: (a -> Bool) -> [a] -> [a]
dropF f s = case s of
  [] -> s
  (x:xs) -> case f x of
    True -> dropF f xs
    False -> s