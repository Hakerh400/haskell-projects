module Util (
  modifyLines,
  normStr,
  padStart,
  padEnd,
  sanl,
  split
) where

import qualified Data.List as List

modifyLines :: (String -> String) -> String -> String
modifyLines f str = List.intercalate "\n" $ map f $ sanl str

normStr :: String -> String
normStr = modifyLines id

padStart :: Int -> String -> String
padStart n str = replicate (max 0 $ n - length str) ' ' ++ str

padEnd :: Int -> String -> String
padEnd 0 a      = a
padEnd n []     = replicate n ' '
padEnd n (x:xs) = x : padEnd (n - 1) xs

sanl :: String -> [String]
sanl = split sanl'

sanl' :: String -> String -> Maybe Int
sanl' _ ('\r':'\n':_) = Just 2
sanl' _ ('\r':_)      = Just 1
sanl' _ ('\n':_)      = Just 1
sanl' _ _             = Nothing

split :: ([a] -> [a] -> Maybe Int) -> [a] -> [[a]]
split func list = split' func 0 [] list [] []

split' :: ([a] -> [a] -> Maybe Int) -> Int -> [a] -> [a] -> [a] -> [[a]] -> [[a]]
split' func skip prev list sub acc = case list of
  [] -> case func prev list of
    Nothing -> reverse (reverse sub : acc)
    _       -> reverse ([] : reverse sub : acc)
  (x:xs) -> case skip of
    0 -> case func prev list of
      Nothing  -> split' func 0 (x : prev) xs (x : sub) acc
      Just len -> case len of
        0 -> split' func 0 (x : prev) xs (x : []) (reverse sub : acc)
        n -> split' func (n - 1) (x : prev) xs [] (reverse sub : acc)
    n -> split' func (n - 1) (x : prev) xs sub acc