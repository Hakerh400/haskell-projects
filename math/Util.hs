module Util
  ( modifyLines
  , normStr
  , padStart
  , padEnd
  , sanl
  , split
  , cwd
  , joinPth
  , normPth
  , fstElem
  , sortPair
  , mapSet
  , filterSet
  , setAsList
  , setAsList'
  , mapPair
  , setGet
  , ex
  , exg
  , isNat
  , str2nat
  , allDigits
  , parens
  , numLines
  , sp
  , lf
  , ss
  , s2
  , u
  , inc
  , dec
  , fromRight
  ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Directory as Dir
import Data.Foldable

import qualified Data.Set as Set
import Data.Set (Set)

modifyLines :: (String -> String) -> String -> String
modifyLines f str = lf $ map f $ sanl str

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

fstElem :: (Foldable t) => t a -> Maybe a
fstElem = find $ const True

sortPair :: (Ord a) => a -> a -> (a, a)
sortPair a b = if a <= b
  then (a, b)
  else (b, a)

mapSet :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
mapSet = setAsList . map

filterSet :: (Ord a) => (a -> Bool) -> Set a -> Set a
filterSet = setAsList . filter

setAsList :: (Ord a, Ord b) => ([a] -> [b]) -> Set a -> Set b
setAsList f = Set.fromList . f . Set.toList

setAsList' :: (Ord a, Ord b) => ([a] -> [b]) -> Set a -> [b]
setAsList' f = f . Set.toList

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a, b) = (f a, f b)

setGet :: (Ord a) => Int -> Set a -> a
setGet i set = Set.toList set !! i

ex :: String -> String
ex expected = "Expected " ++ expected

exg :: String -> String -> String
exg expected got = ex expected ++ ", but got " ++ got

isNat :: String -> Bool
isNat ('0':[]) = True
isNat ('0':_)  = False
isNat xs       = allDigits xs

str2nat :: String -> Either String Int
str2nat str = if isNat str
  then return $ read str
  else Left "Invalid number"

allDigits :: String -> Bool
allDigits str = all Char.isDigit str

parens :: String -> String
parens str = concat ["(", str, ")"]

numLines :: [String] -> String
numLines list = let
  padSize = length $ show $ length list
  in lf $ zipWith (\i s -> concat
    [padStart padSize $ show i, ".", s2, s]
  ) [1..] list

sp :: [String] -> String
sp = ss " "

lf :: [String] -> String
lf = ss "\n"

ss :: [a] -> [[a]] -> [a]
ss = List.intercalate

s2 :: String
s2 = replicate 2 ' '

u :: a
u = undefined

inc :: (Num a) => a -> a
inc a = a + 1

dec :: (Num a) => a -> a
dec a = a - 1

fromRight :: Either a b -> b
fromRight (Right a) = a