import Prelude as P hiding ((*>))

import Data.List as List
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Applicative hiding (empty, (*>))
import Control.Monad

type IntSet = IntSet.IntSet
type Set = Set.Set
type Map = Map.Map

type Lab = Int
type Pos = Int
type PosSet = IntSet
type Recog = Pos -> StateC PosSet
type MapP = Map Pos PosSet
type Context = Map Lab MapP
type StateC = State Context

data State s t = State {unState :: s -> (t, s)}

instance Functor (State s) where
  fmap g (State f) = State $ \s -> let
    (t, s') = f s
    in (g t, s')

instance Applicative (State s) where
  pure t = State (\s -> (t, s))
  liftA2 h (State f) (State g) = State $ \s -> let
    (t1, s1) = f s
    (t2, s2) = g s1
    in (h t1 t2, s2)

instance Monad (State s) where
  (State f) >>= g = State $ \s -> let
    (t, s') = f s
    State g' = g t
    in g' s

evalState :: State s t -> s -> t
evalState (State f) = fst . f

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (const ((), s))

modify :: (s -> s) -> State s ()
modify f = get >>= (put . f)

main :: IO ()
main = do
  putStrLn output
  return ()

str :: String
str = "aaabbbb"

output :: String
output = show $ evalState (recog 0) Map.empty

recog :: Recog
recog = empty <+>
  term "a" *> recog

infixl 3 <+>
(<+>) :: Recog -> Recog -> Recog
(<+>) r1 r2 j = do
  s1 <- r1 j
  s2 <- r2 j
  return $ IntSet.union s1 s2

infixl 4 *>
(*>) :: Recog -> Recog -> Recog
p *> q = \j -> do
  endP <- p j
  endQs <- mapM q (IntSet.elems endP)
  return $ IntSet.unions endQs

empty :: Recog
empty j = return $ IntSet.singleton j

term :: String -> Recog
term tok j = do
  return $ if tok `isPrefixOf` List.drop j str
    then IntSet.singleton $ j + length tok
    else IntSet.empty

memoize :: Lab -> Recog -> Recog
memoize lab f j = do
  ends <- f j
  updateTable lab j ends
  return ends

updateTable :: Lab -> Pos -> PosSet -> StateC ()
updateTable lab j ends = modify $ Map.insertWith
  (uncurry Map.insert . head . Map.toList)
  lab $ Map.singleton j ends