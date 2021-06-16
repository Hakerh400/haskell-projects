module Util (
  modifyLines,
  normStr,
  padStart,
  padEnd,
  sanl,
  split,
  cwd,
  joinPth,
  normPth,
  ex,
  exg,
  isNat,
  allDigits,
  parens,
  sp,
  u
) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Directory as Dir

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

cwd :: IO String
cwd = Dir.getCurrentDirectory

joinPth :: IO String -> String -> IO String
joinPth dir name = do
  d <- dir
  return $ normPth $ d ++ "/" ++ name

normPth :: String -> String
normPth = map $ \c ->
  if c == '\\'
    then '/'
    else c

ex :: String -> String
ex expected = "Expected " ++ expected

exg :: String -> String -> String
exg expected got = ex expected ++ ", but got " ++ got

isNat :: String -> Bool
isNat ('0':[]) = True
isNat ('0':_)  = False
isNat xs       = allDigits xs

allDigits :: String -> Bool
allDigits str = all Char.isDigit str

parens :: String -> String
parens str = concat ["(", str, ")"]

sp :: [String] -> String
sp = List.intercalate " "

u :: a
u = undefined