module Lisparser (
  Node(..),
  Pos(..),
  Elem(..),
  parse,
  formatErr
) where

import qualified Data.Char as Char
import qualified Data.List as List

import ParserMonad

data Node = Node {
  getPos  :: Pos,
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
  getSrc  :: String,
  getPpos :: Pos
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
    getPpos = Pos 0 0
  }

parseMainList :: State Node
parseMainList = parseList True

parseList :: Bool -> State Node
parseList top = do
  trim
  readExactStr $ if top then "" else "("
  list <- parseListItems
  trim
  readExactStr $ if top then "" else ")"
  makeNode $ List list

parseListItems :: State [Node]
parseListItems = do
  trim
  eof <- isEof
  if eof
    then pure []
    else do
      char <- queryChar
      if char == ')'
        then return []
        else do
          node <- parseNode
          nodes <- parseListItems
          return $ node : nodes

parseNode :: State Node
parseNode = do
  char <- queryChar
  if char == '('
    then parseList False
    else parseTerm

parseTerm :: State Node
parseTerm = do
  name <- readWhile $ \c ->
    not $ Char.isSpace c || elem c "()"
  makeNode $ Term name

trim :: State ()
trim = do
  eof <- isEof
  if eof
    then pure ()
    else do
      char <- queryChar
      if Char.isSpace char
        then do
          readChar
          trim
        else pure ()

makeNode :: Elem -> State Node
makeNode e = do
  pos <- gets getPpos
  return $ Node {
    getPos = pos,
    getElem = e
  }

readWhile :: (Char -> Bool) -> State String
readWhile f = do
  eof <- isEof
  if eof
    then return ""
    else do
      char <- queryChar
      if f char
        then do
          readChar
          rest <- readWhile f
          return $ char : rest
        else return ""

readExactStr :: String -> State ()
readExactStr [] = pure ()
readExactStr (x:xs) = do
  pos <- gets getPpos
  c <- readChar
  if c == x
    then readExactStr xs
    else throwErr pos $ concat ["Expected '", [x], "', but found '", [c], "'"]

readChar :: State Char
readChar = readOrQueryChar True

queryChar :: State Char
queryChar = readOrQueryChar False

readOrQueryChar :: Bool -> State Char
readOrQueryChar shouldRead = do
  eof <- isEof
  if eof
    then throwErrHere "Unexpected end of the source code"
    else do
      state <- get
      let (char:rest) = getSrc state
      if shouldRead
        then do
          let (Pos row col) = getPpos state
          let isNewLine = char == '\n'
          put $ ParserState {
            getSrc = rest,
            getPpos = if isNewLine
              then Pos (row + 1) 0
              else Pos row (col + 1)
          }
        else pure ()
      return char

isEof :: State Bool
isEof = do
  src <- gets getSrc
  return $ null src

throwErrHere :: String -> State a
throwErrHere msg = do
  pos <- gets getPpos
  throwErr pos msg

throwErr :: Pos -> String -> State a
throwErr pos msg = throw Error {
    getMsg = msg,
    getErrPos = pos
  }

formatErr :: String -> String -> Error -> String
formatErr file src err = let
  srcLines = lines src
  msg = getMsg err
  (Pos row col) = getErrPos err
  rowStr = show $ row + 1
  colStr = show $ col + 1
  indentSize = max 4 $ length rowStr + 1
  indentStr = indent indentSize
  in concat [
    "\n", file, ":", rowStr, ":", colStr, ": error:\n",
    modifyLines (indentErrMsgLine indentSize) msg, "\n",
    indentStr, "|\n",
    padStart (indentSize - 1) rowStr, " | ", srcLines !! row, "\n",
    indentStr, "| ", replicate col ' ', "^"
  ]

indent :: Int -> String
indent n = replicate n ' '

indentErrMsgLine :: Int -> String -> String
indentErrMsgLine n msg = indent n ++ msg

modifyLines :: (String -> String) -> String -> String
modifyLines f str = List.intercalate "\n" $ map f $ lines str

normStr :: String -> String
normStr = modifyLines id

padStart :: Int -> String -> String
padStart n str = replicate (max 0 $ n - length str) ' ' ++ str

padEnd :: Int -> String -> String
padEnd 0 a      = a
padEnd n []     = replicate n ' '
padEnd n (x:xs) = x : padEnd (n - 1) xs