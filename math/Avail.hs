module Avail
  ( Avail(..)
  , getAvail
  , getAvail'
  ) where

import Data.Foldable
import Data.Maybe

data Avail a = Avail
  { availFirst :: a
  , availNext  :: a -> a
  }

getAvail :: (Foldable t, Eq a) => Avail a -> t a -> a
getAvail avail elems = fromJust $ find
  (not . (`elem` elems)) $
  iterate (availNext avail) (availFirst avail)

getAvail' :: (Foldable t, Eq a) => Avail a -> t a -> a -> a
getAvail' avail elems first = fromJust $ find
  (not . (`elem` elems)) $
  iterate (availNext avail) first