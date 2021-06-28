import Data.Foldable
import Control.Applicative
import Control.Monad

import qualified Data.Set as Set
import Data.Set (Set)

class BoolM m where
  base    :: m a
  wrap    :: Bool -> m a
  unwrap  :: m a -> Bool
  combine :: m a -> m b -> Bool

adapt :: (BoolM m, BoolM m') => m a -> m' b
adapt = wrap . unwrap

newtype Wrap m a = Wrap (m a)

instance (BoolM m) => BoolM (Wrap m) where
  base = Wrap base
  wrap = Wrap . wrap
  unwrap (Wrap a) = unwrap a
  combine (Wrap a) (Wrap b) = combine a b

instance (BoolM m) => Functor (Wrap m) where
  fmap = const adapt

instance (BoolM m) => Applicative (Wrap m) where
  pure = const base
  liftA2 _ a b = wrap $ combine a b

instance (BoolM m) => Monad (Wrap m) where
  return = pure
  a >>= f = wrap $ combine a $ f undefined

instance (BoolM m) => Show (Wrap m a) where
  show a = show $ unwrap a

newtype Conj a = Conj Bool
newtype Disj a = Disj Bool

instance BoolM Conj where
  base = wrap True
  wrap = Conj
  unwrap (Conj a) = a
  combine a b = unwrap a && unwrap b

instance BoolM Disj where
  base = wrap False
  wrap = Disj
  unwrap (Disj a) = a
  combine a b = unwrap a || unwrap b

type Sudoku = [String]
type Test = (Sudoku, Bool)

main :: IO ()
main = print $ all test tests

test :: Test -> Bool
test (sudoku, expected) = unwrap (check sudoku) == expected

tests :: [Test]
tests = [
  ([
    "123456789",
    "456789123",
    "789123456",
    "231564897",
    "564897231",
    "897231564",
    "312645978",
    "645978312",
    "978312645"
  ], True),

  ([
    "472583691",
    "583691472",
    "691472583",
    "725836914",
    "836914725",
    "914725836",
    "258369147",
    "369147258",
    "147258369"
  ], True),

  ([
    "123456789",
    "678912345",
    "234567891",
    "789123456",
    "345678912",
    "891234567",
    "456789123",
    "912345678",
    "567891234"
  ], False)]

n :: Int
n = 3

nn :: Int
nn = n ^ 2

nns :: Char
nns = uni $ show nn

check :: Sudoku -> Wrap Conj Bool
check sudoku = do
  wrap $ length sudoku == nn

  for nn $ \y -> do
    let line = sudoku !! y
    wrap $ length line == nn

    for nn $ \x -> do
      let c = line !! x

      wrap $ c >= '1'
      wrap $ c <= nns

  let get x y = sudoku !! y !! x
  let uni list = wrap $ (Set.size $ Set.fromList list) == nn

  for nn $ \i -> do
    uni $ map (get i) rnn
    uni $ map (flip get i) rnn

  for n $ \i -> do
    for n $ \j -> do
      uni $ do
        x <- rn
        y <- rn
        return $ get (i * n + x) (j * n + y)

for :: (BoolM m) => Int -> (Int -> m a) -> m a
for n f = wrap $ all (unwrap . f) $ rng n

rnn :: [Int]
rnn = rng nn

rn :: [Int]
rn = rng n

rng :: Int -> [Int]
rng n = [0 .. n - 1]

uni :: [a] -> a
uni (a:[]) = a