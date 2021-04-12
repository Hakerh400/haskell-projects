module Set (
  Set,
  fromList,
  setMap,
  setElem
) where

import Data.List as List

instance (Eq a, Show a) => Show (Set a) where
  show (SetCtor a) = "{" ++ List.intercalate ", " (fmap show a) ++ "}"

data Set a = SetCtor [a]

instance (Eq a) => Eq (Set a) where
  SetCtor a == SetCtor b = all (`elem` b) a && all (`elem` a) b

fromList :: (Eq a) => [a] -> Set a
fromList = foldr insert (SetCtor []) where
  insert elm set@(SetCtor elms) = if elm `setElem` set
    then set
    else SetCtor (elm : elms)

setElem :: (Eq a) => a -> Set a -> Bool
a `setElem` (SetCtor b) = a `elem` b

setMap :: (Eq a, Eq b) => (a -> Set b) -> Set a -> Set b
setMap f (SetCtor elms) = foldr (union . f) (SetCtor []) elms where
  union (SetCtor a) (SetCtor b) = fromList (a ++ b)