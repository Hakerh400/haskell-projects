import Prelude hiding ((*>))

import Data.List
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import qualified Control.Monad.State as S
import Control.Monad.State hiding (State)

type IntSet = IntSet.IntSet
type Map = Map.Map

type Lab = Int
type Pos = Int
type Ends = IntSet
type MapE = Map Pos Ends
type MTable = Map Lab MapE
type Recog = Pos -> State Ends

type State = S.State MTable

main :: IO ()
main = do
  print output
  return ()

input :: String
input = "abcd"

output :: Ends
output = evalState (recog 0) Map.empty

recog :: Recog
recog = term "a" *> term "b"

infixl 3 <+>
(<+>) :: Recog -> Recog -> Recog
r1 <+> r2 = \j -> do
  ends1 <- r1 j
  ends2 <- r2 j
  return $ IntSet.union ends1 ends2

infixl 4 *>
(*>) :: Recog -> Recog -> Recog
r1 *> r2 = \j -> do
  ends1 <- r1 j
  ends2 <- mapM r2 $ IntSet.elems ends1
  return $ IntSet.unions ends2

empty :: Recog
empty j = do
  return $ IntSet.singleton j

term :: String -> Recog
term str j = do
  return $ if str `isPrefixOf` drop j input
    then IntSet.singleton $ j + (length str)
    else IntSet.empty

lookupT :: Lab -> Pos -> State (Maybe Ends)
lookupT lab j = do
  mt <- get
  return $ Map.lookup lab mt >>= Map.lookup j

memoize :: (Enum lab) => lab -> Recog -> Recog
memoize labE r = \j -> do
  let lab = fromEnum labE
  res <- lookupT lab j
  case res of
    Nothing -> do
      ends <- r j
      updateTable lab j ends
    Just ends -> return ends

updateTable :: Lab -> Pos -> Ends -> State Ends
updateTable lab j ends = do
  mt <- get
  let newm = Map.singleton j ends
  put $ Map.insertWith insertWithFunc lab newm mt
  return ends

insertWithFunc :: MapE -> MapE -> MapE
insertWithFunc new old = let
  (k, v) = head $ Map.toList new
  in Map.insert k v old