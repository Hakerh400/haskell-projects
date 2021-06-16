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

type M = Either Error

type Program = String

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

  case parseAndMakeProg file src of
    Left  err  -> putStrLn $ show err
    Right prog -> putStrLn prog

parseAndMakeProg :: String -> String -> M Program
parseAndMakeProg file src = do
  parsed <- Parser.parse file src
  prog <- makeProg parsed
  return prog

makeProg :: Node -> M Program
makeProg node = do
  elems <- L.elems node

  let mainPred = node {
    getElem = List $
      node {getElem = Ident "->"}
      : elems}

  p <- parsePred mainPred
  
  return $ show mainPred

parsePred :: Node -> M Pred
parsePred node = do
  isIdent <- L.s node
  if isIdent
    then do
      name <- L.m node
      case name of
        "T" -> return PTrue
        "F" -> return PFalse
        a   -> L.err node $ "Undefined constant " ++ show a
    else do
      t <- L.t node
      elems <- L.elems' node 1
      case t of
        "all" -> parseQuantifier Forall node
        "exi" -> parseQuantifier Exists node
        "<->" -> parseStruct Equiv elems
        "->"  -> parseStruct Impl  elems
        "|"   -> parseStruct Disj  elems
        "&"   -> parseStruct Conj  elems
        "~"   -> do
          L.len node 2
          p <- L.e node 1 >>= parsePred
          return $ Neg p
        "\\"  -> do
          L.len node 2
          expr <- L.e node 1 >>= parseExpr
          return $ Stat expr
        _     -> do
          a <- L.fst node
          L.err a $ "Undefined structure " ++ show t

parseStruct :: (Set Pred -> Pred) -> [Node] -> M Pred
parseStruct f elems = do
  xs <- mapM parsePred elems
  return $ f $ Set.fromList xs

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
      name <- L.t node
      elems <- L.elems' 1
      foldlM (\a b -> do
        c <- parseExpr b
        return $ ExprP a c
        ) (ExprI name) elems

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