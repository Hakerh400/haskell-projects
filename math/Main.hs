import qualified Parser
import qualified Lisp as L
import Types
import Error
import Util
import MonadE
import State
import Predicate

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable

type System = String

type State s = StateT s Error
type M = Either Error

srcDir :: IO String
srcDir = joinPth cwd "src"

sysFile :: IO String
sysFile = joinPth srcDir "system.txt"

main :: IO ()
main = do
  srcDirPth <- srcDir
  filePth <- sysFile

  let file = drop (length srcDirPth + 1) filePth
  src <- readFile filePth

  case parseAndParseSys file src of
    Left  err  -> putStrLn $ show err
    Right sys  -> putStrLn sys

parseAndParseSys :: String -> String -> M System
parseAndParseSys file src = do
  parsed <- Parser.parse file src
  sys <- parseSys parsed
  return sys

parseSys :: Node -> M System
parseSys node = do
  elems <- L.elems node

  let mainPred = node {
    getElem = List $
      node {getElem = Ident "->"}
      : elems}

  p <- parsePred mainPred
  
  return $ show p

parsePred :: Node -> M Pred
parsePred node = do
  isIdent <- L.s node
  if isIdent
    then do
      name <- L.m node
      case name of
        "T" -> return PTrue
        "F" -> return PFalse
        a   -> parseStat node
    else do
      t <- L.t node
      elems <- L.elems' node 1
      case t of
        "all" -> parseQuantifier Forall node
        "exi" -> parseQuantifier Exists node
        "<->" -> do
          L.len node 3
          a <- L.e node 1 >>= parsePred
          b <- L.e node 2 >>= parsePred
          return $ Equiv a b
        "->"  -> parseStruct Impl node
        "|"   -> parseStruct Disj node
        "&"   -> parseStruct Conj node
        "~"   -> do
          L.len node 2
          p <- L.e node 1 >>= parsePred
          return $ Neg p
        _     -> parseStat node

parseStat :: Node -> M Pred
parseStat node = do
  expr <- parseExpr node
  return $ Stat expr

parseStruct :: (Pred -> Pred -> Pred) -> Node -> M Pred
parseStruct f node = do
  L.lenp node 2
  elems <- L.elems' node 1
  xs <- mapM parsePred elems
  return $ foldr1 f xs

parseQuantifier :: (String -> Pred -> Pred) -> Node -> M Pred
parseQuantifier f node = do
  L.len node 3
  name <- L.e node 1 >>= getVar
  p <- L.e node 2 >>= parsePred
  return $ f name p

parseExpr :: Node -> M Expr
parseExpr node = do
  isIdent <- L.s node
  if isIdent
    then do
      name <- L.m node
      return $ ExprI name
    else do
      L.lenp node 1
      elems <- L.elems node
      xs <- mapM parseExpr elems
      return $ foldl1 ExprP xs

getVar :: Node -> M String
getVar node = getIdent "variable"
  (\name -> let
    (c:cs) = name
    in Char.isLower c && all Char.isAlphaNum cs)
  node

getIdent :: String -> (String -> Bool) -> Node -> M String
getIdent k f node = do
  name <- L.m node
  if f name
    then return name
    else L.err node $ concat
      [show name, " is not a valid ", k, " name"]