module Avail
  ( Avail(..)
  , getAvail
  , getAvail'
  , getAvails
  ) where

import Data.Foldable
import Data.Maybe

import qualified Data.Set as Set
import Data.Set (Set)

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

getAvails :: (Ord a) => (Set a -> a) -> Set a -> [a]
getAvails f elems = let
  a = f elems
  elems' = Set.insert a elems
  in a : getAvails f elems'