import Data.List
import Data.Maybe
import Control.Monad.State

import Prelude hiding (read)

import Base
import Tree
import Serializer

k = 10

main :: IO ()
main = mapM_ func [0..100] where
    func n = do
      let t = deser k n
      let n1 = ser k t
      putStrLn $ concat [show n, " -> ", show t]
      if n1 == n
        then pure ()
        else error $ show n1

serInit :: N -> N -> SerT
serInit k n = SerT
  { num = n
  , stack = []
  , table = map Leaf [0..k-1]
  }

ser :: N -> Tree -> N
ser n t = evalState (serTree False Nothing t >> getOutput) $ serInit n 0

deser :: N -> N -> Tree
deser n nat = evalState (deserTree False Nothing) $ serInit n nat

serTree :: Bool -> Maybe Tree -> Tree -> Ser ()
serTree more mleft t = do
  table <- getTable
  ts <- pure $ case mleft of
    Just left -> filter filterFunc table where
      filterFunc right = not $ Node left right `elem` table
    Nothing -> table
  let n = len ts
  case getIndex t ts of
    Just i -> do
      if more
        then do
          write2 0
          write n i
        else inc i
    Nothing -> do
      let Node left right = t
      if more
        then write2 1
        else inc n
      serTree True Nothing left
      serTree more (Just left) right
      push t

deserTree :: Bool -> Maybe Tree -> Ser Tree
deserTree more mleft = do
  table <- getTable
  ts <- pure $ case mleft of
    Just left -> filter filterFunc table where
      filterFunc right = not $ Node left right `elem` table
    Nothing -> table
  let n = len ts
  if more
    then do
      b <- read2
      case b of
        0 -> do
          i <- read n
          return $ getElem ts i
        1 -> do
          left <- deserTree True Nothing
          right <- deserTree more (Just left)
          let t = Node left right
          push t
          return t
    else do
      mi <- lt n
      case mi of
        Just i -> return $ getElem ts i
        Nothing -> do
          left <- deserTree True Nothing
          right <- deserTree more (Just left)
          let t = Node left right
          push t
          return t

  -- case getIndex t ts of
  --   Just i -> do
  --     if more
  --       then write2 0
  --       else pure ()
  --     write n i
  --   Nothing -> do
  --     let Node left right = t
  --     if more
  --       then write2 1
  --       else inc n
  --     deserTree False Nothing left
  --     deserTree more (Just left) right

push :: Tree -> Ser ()
push t = do
  table <- getTable
  setTable $ table ++ [t]

getIndex :: (Eq a) => a -> [a] -> Maybe N
getIndex x xs = fmap toInteger $ elemIndex x xs

getElem :: [a] -> N -> a
getElem (x:_)  0 = x
getElem (_:xs) n = getElem xs $ n - 1

len :: [a] -> N
len xs = toInteger $ length xs

u :: a
u = undefined