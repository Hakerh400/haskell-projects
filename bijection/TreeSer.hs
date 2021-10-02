module TreeSer
  ( ser
  , deser
  ) where

import Data.List
import Data.Maybe
import Data.Foldable
import Control.Applicative
import Control.Monad
import Control.Monad.State

import Prelude hiding (read)

import Base
import Tree
import Serializer

initSer :: Table -> N -> SerT
initSer table n = SerT
  { num = n
  , stack = []
  , table = table
  , dmax = (foldr (liftA2 max) (Just 0) $ map snd table) >>= Just . id
  }

ser :: Table -> Tree -> N
ser table t = evalState (serTree False Nothing 0 t >> getOutput) $
  initSer table 0

deser :: Table -> N -> Tree
deser table nat = evalState (deserTree False Nothing 0) $
  initSer table nat

serTree :: Bool -> Maybe Tree -> N -> Tree -> Ser ()
serTree more mleft depth t = do
  dmax <- getDmax
  ifnDmax <- pure $ \action -> case dmax of
    Nothing -> action
    Just m -> if depth < m
      then action
      else pure ()
  table <- getTableWithDepth depth
  ts <- pure $ case mleft of
    Just left -> filter filterFunc table where
      filterFunc (right, _) = not $ any ((== Node left right) . fst) table
    Nothing -> table
  let n = len ts
  case getIndex t ts of
    Just i -> do
      if more
        then do
          ifnDmax $ write2 0
          write n i
        else inc i
    Nothing -> do
      let Node left right = t
      ifnDmax $ if more
        then write2 1
        else inc n
      serTree True Nothing (depth + 1) left
      serTree more (Just left) 0 right
      tableRaw <- getTable
      let Just (_, depthLeft) = find ((== left) . fst) tableRaw
      push (t, depthLeft >>= Just . dec)

deserTree :: Bool -> Maybe Tree -> N -> Ser Tree
deserTree more mleft depth = do
  dmax <- getDmax
  isDmax <- pure $ maybe False (== depth) dmax
  table <- getTableWithDepth depth
  ts <- pure $ case mleft of
    Just left -> filter filterFunc table where
      filterFunc (right, _) = not $ any ((== Node left right) . fst) table
    Nothing -> table
  let n = len ts
  deserRec <- pure $ do
    left <- deserTree True Nothing (depth + 1)
    right <- deserTree more (Just left) 0
    let t = Node left right
    tableRaw <- getTable
    let Just (_, depthLeft) = find ((== left) . fst) tableRaw
    push (t, depthLeft >>= Just . dec)
    return t
  retElem <- pure $ \i -> do
    if i < len ts
      then return $ fst $ getElem ts i
      else error $ show $ n
  if more
    then do
      b <- if isDmax
        then pure 0
        else read2
      case b of
        0 -> do
          i <- read n
          retElem i
        1 -> deserRec
    else do
      mi <- if isDmax
        then do
          num <- getNum
          return $ if num < n
            then Just num
            else Nothing
        else lt n
      case mi of
        Just i -> retElem i
        Nothing -> deserRec

getTableWithDepth :: N -> Ser Table
getTableWithDepth depth = do
  table <- getTable
  return $ filter (filterTableWithDepth depth) table

filterTableWithDepth :: N -> (Tree, Maybe N) -> Bool
filterTableWithDepth depth e = maybe True (>= depth) $ snd e

push :: (Tree, Maybe N) -> Ser ()
push t = do
  table <- getTable
  setTable $ table ++ [t]

getIndex :: Tree -> Table -> Maybe N
getIndex = getIndex' 0

getIndex' :: N -> Tree -> Table -> Maybe N
getIndex' _ _ [] = Nothing
getIndex' i x ((y,_):ys) = if y == x
  then Just i
  else getIndex' (i + 1) x ys

getElem :: [a] -> N -> a
getElem (x:_)  0 = x
getElem (_:xs) n = getElem xs $ n - 1