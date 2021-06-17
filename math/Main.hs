import qualified Data.Char as Char
import qualified Data.List as List
import Data.Foldable
import Control.Monad

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Parser
import qualified Lisp as L
import Types
import Error
import Util
import MonadE
import State
import Predicate

type State  s = StateT s Error
type StateP   = State  InfoP
type M        = Either Error
type Quantifier = String -> Pred -> Pred

data InfoP = InfoP {
  getIdentsSet :: Set String
}

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

  case parseAndInitSys file src of
    Left  err  -> putStrLn $ show err
    Right sys  -> print sys

parseAndInitSys :: String -> String -> M Pred
parseAndInitSys file src = do
  let func = parseAndInitSys' file src
  let state = InfoP {
    getIdentsSet = Set.empty
  }
  evalState func state

parseAndInitSys' :: String -> String -> StateP Pred
parseAndInitSys' file src = do
  parsed <- either2state $ Parser.parse file src
  sys <- parseSys parsed
  initSys sys

-- Parse

parseSys :: Node -> StateP Pred
parseSys node = do
  elems <- L.elems node
  let mainNode = node {
    getElem = List $
      node {getElem = Ident "->"}
      : elems}
  parsePred mainNode

parsePred :: Node -> StateP Pred
parsePred node = do
  isIdent <- L.s node
  if isIdent
    then do
      name <- L.m node
      case name of
        "True"  -> return Ptrue
        "False" -> return Pfalse
        a   -> parseStat node
    else do
      t <- L.t node
      elems <- L.elems' node 1
      case t of
        "all" -> parseQuantifier Forall node
        "exi" -> parseQuantifier Exists node
        "->"  -> parseStruct Impl node
        "<->" -> do
          L.len node 3
          a <- L.e node 1 >>= parsePred
          b <- L.e node 2 >>= parsePred
          return $ Equiv a b
        "|"   -> parseStruct Or node
        "&"   -> parseStruct And node
        "~"   -> do
          L.len node 2
          p <- L.e node 1 >>= parsePred
          return $ Pnot p
        _     -> parseStat node

parseStat :: Node -> StateP Pred
parseStat node = do
  expr <- parseExpr node
  return $ Stat expr

parseStruct :: (Pred -> Pred -> Pred) -> Node -> StateP Pred
parseStruct f node = do
  L.lenp node 2
  elems <- L.elems' node 1
  xs <- mapM parsePred elems
  return $ foldr1 f xs

parseQuantifier :: Quantifier -> Node -> StateP Pred
parseQuantifier f node = do
  L.len node 3
  names <- L.e node 1 >>= getQuantifierIdents
  unfoldQuantifierIdents f names $
    L.e node 2 >>= parsePred

unfoldQuantifierIdents :: Quantifier -> [String] -> StateP Pred -> StateP Pred
unfoldQuantifierIdents f []           sp = sp
unfoldQuantifierIdents f (name:names) sp = do
  state <- get
  let idents = getIdentsSet state
  put state {getIdentsSet = Set.insert name idents}
  
  p <- unfoldQuantifierIdents f names sp
  return $ f name p

getQuantifierIdents :: Node -> StateP [String]
getQuantifierIdents node = do
  isIdent <- L.s node
  if isIdent
    then do
      name <- getIdent node
      return [name]
    else do
      elems <- L.elems node
      mapM getIdent elems

parseExpr :: Node -> StateP Expr
parseExpr node = do
  isIdent <- L.s node
  if isIdent
    then do
      name <- getIdent node
      idents <- gets getIdentsSet

      if name `elem` idents
        then return ()
        else L.err node $ "Undefined identifier " ++ show name

      return $ ExprI name
    else do
      L.lenp node 1
      elems <- L.elems node
      xs <- mapM parseExpr elems
      return $ foldl1 ExprP xs

getIdent :: Node -> StateP String
getIdent node = do
  name <- L.m node
  if isBuiltinPred name
    then L.err node $ exg "an identifier" "a predicate"
    else return name

-- Init

initSys :: Pred -> StateP Pred
initSys sys = do
  put $ InfoP {getIdentsSet = Set.empty}

  -- 1. Eliminate biconditionals and implications
  sys <- return $ elimBci sys

  -- 2. Move negation inwards
  sys <- return $ moveNeg sys

  -- 3. Standardize variables apart by renaming them
  --    Each quantifier should use a different variable
  sys <- standardizeIdents sys

  return sys

elimBci :: Pred -> Pred
elimBci (Forall a b) = Forall a $ elimBci b
elimBci (Exists a b) = Exists a $ elimBci b
elimBci (Impl   a b) = Pnot (elimBci a) `Or` elimBci b
elimBci (Equiv  a b) = (a' `And` b') `Or` (Pnot a' `And` Pnot b')
  where a' = elimBci a
        b' = elimBci b
elimBci (Or     a b) = elimBci a `Or`  elimBci b
elimBci (And    a b) = elimBci a `And` elimBci b
elimBci (Pnot   a  ) = Pnot $ elimBci a
elimBci a            = a

moveNeg :: Pred -> Pred
moveNeg (Forall a b) = Forall a $ moveNeg b
moveNeg (Exists a b) = Exists a $ moveNeg b
moveNeg (Or     a b) = moveNeg a `Or`  moveNeg b
moveNeg (And    a b) = moveNeg a `And` moveNeg b
moveNeg (Pnot   a  ) = moveNeg' a
moveNeg a            = a

moveNeg' :: Pred -> Pred
moveNeg' (Forall a b) = Exists a $ moveNeg $ Pnot b
moveNeg' (Exists a b) = Forall a $ moveNeg $ Pnot b
moveNeg' (Or     a b) = moveNeg (Pnot a) `And` moveNeg (Pnot b)
moveNeg' (And    a b) = moveNeg (Pnot a) `Or`  moveNeg (Pnot b)
moveNeg' (Pnot   a  ) = moveNeg a
moveNeg' (Ptrue     ) = Pfalse
moveNeg' (Pfalse    ) = Ptrue
moveNeg' a@(Stat _  )  = Pnot a

standardizeIdents :: Pred -> StateP Pred
standardizeIdents (Forall a b) = standardizeIdent Forall a b
standardizeIdents (Exists a b) = standardizeIdent Exists a b
standardizeIdents (Or     a b) = liftM2 Or  (standardizeIdents a) (standardizeIdents b)
standardizeIdents (And    a b) = liftM2 And (standardizeIdents a) (standardizeIdents b)
standardizeIdents a@(Stat _  ) = return a
standardizeIdents a = error $ show a

standardizeIdent :: Quantifier -> String -> Pred -> StateP Pred
standardizeIdent f name p = do
  state <- get
  let idents = getIdentsSet state

  (name, p) <- if name `elem` idents
    then do
      let name' = getAvailIdentFromSet idents
      let p'    = renameIdentP name name' p
      return (name', p')
    else return (name, p)

  put $ state {getIdentsSet = Set.insert name idents}
  p <- standardizeIdents p

  return $ f name p

renameIdentP :: String -> String -> Pred -> Pred
renameIdentP x y (Forall a b) = Forall a $ renameQuantifier x y a b
renameIdentP x y (Exists a b) = Exists a $ renameQuantifier x y a b
renameIdentP x y (Stat   a  ) = Stat $ renameIdentE x y a
renameIdentP x y a = error $ show a

renameIdentE :: String -> String -> Expr -> Expr
renameIdentE x y (ExprI a) = if a == x
  then ExprI y
  else ExprI a
renameIdentE x y (ExprP a b) = ExprP (renameIdentE x y a) (renameIdentE x y b)

renameQuantifier :: String -> String -> String -> Pred -> Pred
renameQuantifier x y a p = if a == x
  then p
  else renameIdentP x y p

getAvailIdentFromSet :: Set String -> String
getAvailIdentFromSet = getAvailIdentFromList . Set.toList

getAvailIdentFromList :: [String] -> String
getAvailIdentFromList []     = firstIdent
getAvailIdentFromList (x:xs) = if x == firstIdent
  then getAvailIdentFromList' x xs
  else firstIdent

getAvailIdentFromList' :: String -> [String] -> String
getAvailIdentFromList' ident []     = nextIdent ident
getAvailIdentFromList' ident (x:xs) = if x == nextIdent ident
  then getAvailIdentFromList' x xs
  else nextIdent ident

firstIdent :: String
firstIdent = "a"

nextIdent :: String -> String
nextIdent = reverse . nextIdent' . reverse

nextIdent' :: String -> String
nextIdent' []     = "a"
nextIdent' (c:cs) = if c == 'z'
  then 'a' : nextIdent' cs
  else nextIdentChar c : cs

nextIdentChar :: Char -> Char
nextIdentChar c = Char.chr $ Char.ord c + 1