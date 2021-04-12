module Common where

import Data.List

u :: a
u = undefined

loop :: a
loop = loop

ite :: Bool -> a -> a -> a
ite True a b = a
ite False a b = b

itn :: Bool -> IO () -> IO ()
itn a b = ite a b nop

ifd :: Maybe a -> (a -> IO ()) -> IO ()
ifd m f = maybe nop f m

imte :: Maybe a -> (a -> IO b) -> IO b -> IO b
imte m f z = maybe z f m

nop :: IO ()
nop = pure ()

sjoin :: [a] -> [[a]] -> [a]
sjoin a b = concat $ intersperse a b

slist :: [String] -> String
slist a = "[" ++ sjoin ", " a ++ "]"