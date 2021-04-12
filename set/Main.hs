import Set

main :: IO ()
main = print output

x = fromList [1, 2, 3, 4, 5]

output = partition (<= 3) x

partition :: (Eq a) => (a -> Bool) -> Set a -> (Set a, Set a)
partition f set = (sfilter f set, sfilter (not . f) set)

sfilter :: (Eq a) => (a -> Bool) -> Set a -> Set a
sfilter f = setMap func where
  func elm = if f elm
    then singleton elm
    else empty

disjointUnion :: (Eq a, Eq b) => Set a -> Set b -> Set (Either a b)
disjointUnion set1 set2 = union (smap Left set1) (smap Right set2)

smap :: (Eq a, Eq b) => (a -> b) -> Set a -> Set b
smap f = setMap (singleton . f)

disjoint :: (Eq a) => Set a -> Set a -> Bool
disjoint set1 set2 = intersect set1 set2 == empty

cartesianProduct :: (Eq a, Eq b) => Set a -> Set b -> Set (a, b)
cartesianProduct set1 set2 = setMap func1 set1 where
  func1 elm1 = smap func2 set2 where
    func2 elm2 = (elm1, elm2)

isSubsetOf :: (Eq a) => Set a -> Set a -> Bool
isSubsetOf set1 set2 = every (`setElem` set2) set1

isProperSubsetOf :: (Eq a) => Set a -> Set a -> Bool
isProperSubsetOf set1 set2 = set1 /= set2 && isSubsetOf set1 set2

powerSet :: (Eq a) => Set a -> Set (Set a)
powerSet set = insert set $ if isEmpty set
  then empty
  else setMap func set
  where
    func elm = powerSet $ remove elm set

empty :: (Eq a) => Set a
empty = fromList []

singleton :: (Eq a) => a -> Set a
singleton a = fromList [a]

upair :: (Eq a) => a -> a -> Set a
upair a b = fromList [a, b]

isEmpty :: (Eq a) => Set a -> Bool
isEmpty set = set == empty

invert :: (Eq a, Eq b) => Set a -> Set (Set b)
invert set = if isEmpty set
  then singleton empty
  else empty

unions :: (Eq a) => Set (Set a) -> Set a
unions = setMap id

union :: (Eq a) => Set a -> Set a -> Set a
union a b = unions (upair a b)

insert :: (Eq a) => a -> Set a -> Set a
insert elm = union (singleton elm)

some :: (Eq a) => (a -> Bool) -> Set a -> Bool
some f set = setMap func set /= empty where
  func elm = if f elm
    then singleton elm
    else empty

every :: (Eq a) => (a -> Bool) -> Set a -> Bool
every f = not . some (not . f)

intersect :: (Eq a) => Set a -> Set a -> Set a
intersect set1 set2 = setMap func set1 where
  func elm = if elm `setElem` set2
    then singleton elm
    else empty

dif :: (Eq a) => Set a -> Set a -> Set a
dif set1 set2 = setMap func set1 where
  func elm = if not (elm `setElem` set2)
    then singleton elm
    else empty

remove :: (Eq a) => a -> Set a -> Set a
remove elm set = dif set (singleton elm)

size :: (Eq a) => Set a -> Integer
size set = if isEmpty set
  then 0
  else 1 + extractInt (setMap func set)
  where
    func elm = singleton $ size $ remove elm set

extractInt :: Set Integer -> Integer
extractInt set = extractInt' set 0 where
  extractInt' set n = if set == singleton n
    then n
    else extractInt' set (n + 1)