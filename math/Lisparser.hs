module Lisparser (
  Node(..),
  Pos(..),
  Elem(..),
  parse,
  formatErr
) where

import qualified Data.Char as Char
import qualified Data.List as List

import ParserState

data Node = Node {
  getPos  :: Pos,
  getElem :: Elem
}

data Pos = Pos {
  getRow   :: Int,
  getCol   :: Int,
  getIndex :: Int
}

data Elem =
  Term String |
  List [Node]

data ParserState = ParserState {
  getSrc       :: String,
  getParserPos :: Pos
}

data Error = Error {
  getMsg    :: String,
  getErrPos :: Pos
}

instance Show Node where
  show = show . getElem

instance Show Elem where
  show (Term name) = name
  show (List list) = "(" ++ List.intercalate " " (fmap show list) ++ ")"

type State = StateT ParserState Error

parse :: String -> Either Error Node
parse src = evalState parseMainList
  ParserState {
    getSrc = normStr src,
    getParserPos = Pos 0 0 0
  }

parseMainList :: State Node
parseMainList = parseList True

parseList :: Bool -> State Node
parseList top = do
  trim
  throw $ Error {
    getMsg = "Message 1\nMessage 2",
    getErrPos = Pos 21 2 0
  }

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
  src <- gets getSrc
  return $ null src

readChar :: State Char
readChar = readOrQueryChar True

queryChar :: State Char
queryChar = readOrQueryChar False

readOrQueryChar :: Bool -> State Char
readOrQueryChar shouldRead = do
  state <- get
  let (char:rest) = getSrc state
  if shouldRead
    then put state {getSrc = rest}
    else return ()
  return char

formatErr :: String -> String -> Error -> String
formatErr file src err = let
  srcLines = lines src
  msg = getMsg err
  pos = getErrPos err
  row = getRow pos
  col = getCol pos
  rowStr = show $ row + 1
  colStr = show $ col + 1
  indentSize = length rowStr + 1
  indentStr = indent indentSize
  in concat [
    "\n", file, ":", rowStr, ":", colStr, ": error:\n",
    modifyLines (indentErrMsgLine indentSize) msg, "\n",
    indentStr, "|\n",
    padEnd indentSize rowStr, "| ", srcLines !! row, "\n",
    indentStr, "| ", replicate col ' ', "^"
  ]

indent :: Int -> String
indent n = replicate n ' '

indentErrMsgLine :: Int -> String -> String
indentErrMsgLine n msg = indent n ++ "* " ++ msg

modifyLines :: (String -> String) -> String -> String
modifyLines f str = List.intercalate "\n" $ map f $ lines str

normStr :: String -> String
normStr = modifyLines id

padEnd :: Int -> String -> String
padEnd 0 a = a
padEnd n [] = replicate n ' '
padEnd n (x:xs) = x : padEnd (n - 1) xs