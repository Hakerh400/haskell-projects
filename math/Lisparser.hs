module Lisparser (
  Node(..),
  Pos(..),
  Elem(..),
  parse
) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Control.Monad.State as State

data Node = Node {
  getPos :: Pos,
  getElem :: Elem
}

data Pos = Pos {
  getRow :: Int,
  getCol :: Int
}

data Elem =
  Term String |
  List [Node]

data ParserState = ParserState {
  getSrc :: String,
  getParserPos :: Pos
}

instance Show Node where
  show = show . getElem

instance Show Elem where
  show (Term name) = name
  show (List list) = "(" ++ List.intercalate " " (fmap show list) ++ ")"

type State = State.State ParserState

parse :: String -> Node
parse src = State.evalState parseMainList ParserState {
  getSrc = src,
  getParserPos = Pos 0 0}

parseMainList :: State Node
parseMainList = parseList True

parseList :: Bool -> State Node
parseList top = do
  trim
  return undefined

trim :: State ()
trim = do
  eof <- isEof
  if eof
    then return ()
    else do
      char <- queryChar
      if Char.isSpace char
        then do
          readChar
          trim
        else
          return ()

isEof :: State Bool
isEof = do
  src <- State.gets getSrc
  return $ null src

readChar :: State Char
readChar = readOrQueryChar True

queryChar :: State Char
queryChar = readOrQueryChar False

readOrQueryChar :: Bool -> State Char
readOrQueryChar shouldRead = do
  state <- State.get
  let (char:rest) = getSrc state
  if shouldRead
    then State.put state {getSrc = rest}
    else return ()
  return char