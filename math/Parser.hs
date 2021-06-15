module Parser (
  parse
) where

import qualified Data.Char as Char
import qualified Data.List as List

import State
import Types
import Error
import Util
import MonadE

data ParserState = ParserState {
  getPfile :: File,
  getPsrc  :: String,
  getPpos  :: Pos
}

type State = StateT ParserState Error

parse :: String -> String -> Either Error Node
parse file src = let
  normSrc = normStr src
  in evalState parseMainList
    ParserState {
      getPfile = File {
        getFname = file,
        getFsrc  = normSrc
      },
      getPsrc = normSrc,
      getPpos = Pos 0 0
    }

parseMainList :: State Node
parseMainList = parseList True

parseList :: Bool -> State Node
parseList top = do
  trim
  pos <- gets getPpos
  pos <- if top
    then return $ pos {getCol = (-1)}
    else do
      readExactChar '('
      return pos
  list <- parseListItems
  trim
  if top
    then do
      eof <- isEof
      if eof
        then return ()
        else throwErrHere "Missing open parenthese"
    else readExactChar ')'
  makeNode pos $ List list

parseListItems :: State [Node]
parseListItems = do
  trim
  eof <- isEof
  if eof
    then return []
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
    else parseIdent

parseIdent :: State Node
parseIdent = do
  pos <- gets getPpos
  name <- readWhile $ \c ->
    not $ Char.isSpace c || elem c "()"
  makeNode pos $ Ident name

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
        else return ()

makeNode :: Pos -> Elem -> State Node
makeNode pos e = do
  file <- gets getPfile
  return $ Node {
    getFile = file,
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
readExactStr [] = return ()
readExactStr (c:cs) = do
  readExactChar c
  readExactStr cs

readExactChar :: Char -> State ()
readExactChar c = do
  pos <- gets getPpos
  ch <- readChar
  if ch == c
    then return ()
    else throwErr pos $ exg (show c) (show ch)

readChar :: State Char
readChar = readOrQueryChar True

queryChar :: State Char
queryChar = readOrQueryChar False

readOrQueryChar :: Bool -> State Char
readOrQueryChar shouldRead = do
  eof <- isEof
  if eof
    then throwErrHere "Missing closed parenthese"
    else do
      state <- get
      let (char:rest) = getPsrc state
      if shouldRead
        then do
          let (Pos row col) = getPpos state
          let isNewLine = char == '\n'
          put $ state {
            getPsrc = rest,
            getPpos = if isNewLine
              then Pos (row + 1) 0
              else Pos row (col + 1)
          }
        else return ()
      return char

isEof :: State Bool
isEof = do
  src <- gets getPsrc
  return $ null src

throwErrHere :: String -> State a
throwErrHere msg = do
  pos <- gets getPpos
  throwErr pos msg

throwErr :: Pos -> String -> State a
throwErr pos msg = do
  file <- gets getPfile
  throw Error {
    getErrFile = file,
    getErrPos = pos,
    getMsg = msg
  }