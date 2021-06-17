import qualified Data.Char as Char
import qualified Data.List as List
import Data.Foldable
import Control.Monad
import System.IO

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
import CNF

type State  s = StateT s Error
type StateP   = State  InfoP
type M        = Either Error
type Quantifier = String -> Pred -> Pred

data InfoP = InfoP
  { getConstsSet :: Set String
  , getVarsSet   :: Set String
  }

srcDir :: IO String
srcDir = joinPth cwd "src"

sysFile :: IO String
sysFile = joinPth srcDir "system.txt"

main :: IO ()
main = do
  mapM_ (flip hSetBuffering NoBuffering)
    [stdin, stdout, stderr]

  srcDirPth <- srcDir
  filePth <- sysFile

  let file = drop (length srcDirPth + 1) filePth
  src <- readFile filePth

  case parseAndInitSys file src of
    Left  err  -> putStrLn $ show err
    Right sys  -> prove $ pred2cnf sys

prove :: CNF -> IO ()
prove cnf = if isCnfProved cnf
  then print "\n---\n\nProved!"
  else do
    print cnf
    putStr "\n> "
    line <- getLine
    putStrLn "\n"

    return ()

parseAndInitSys :: String -> String -> M Pred
parseAndInitSys file src = do
  let func = parseAndInitSys' file src
  let state = InfoP {
    getVarsSet   = Set.empty,
    getConstsSet = Set.empty}
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
  addVar name
  p <- unfoldQuantifierIdents f names sp
  return $ f name p

getQuantifierIdents :: Node -> StateP [String]
getQuantifierIdents node = do
  isIdent <- L.s node
  if isIdent
    then do
      name <- getVar node
      return [name]
    else do
      elems <- L.elems node
      mapM getVar elems

parseExpr :: Node -> StateP Expr
parseExpr node = do
  isIdent <- L.s node
  if isIdent
    then do
      name <- getIdent node
      let identType = getIdentType name
      
      case identType of
        Const -> addConst name
        Var   -> do
          idents <- gets getVarsSet
          if name `elem` idents
            then return ()
            else L.err node $ "Undefined variable " ++ show name

      return $ ExprI identType name
    else do
      L.lenp node 1
      elems <- L.elems node
      xs <- mapM parseExpr elems
      return $ foldl1 ExprP xs

getVar :: Node -> StateP String
getVar node = do
  name <- getIdent node
  case getIdentType name of
    Const -> L.err node $ concat
      [show name, " is not a valid variable name"]
    Var -> return name

getIdent :: Node -> StateP String
getIdent node = do
  name <- L.m node
  if isBuiltinPred name
    then L.err node $ exg "an identifier" "a predicate"
    else return name

addConst :: String -> StateP ()
addConst name = do
  state <- get
  let idents = getConstsSet state
  put state {getConstsSet = Set.insert name idents}

addVar :: String -> StateP ()
addVar name = do
  state <- get
  let idents = getVarsSet state
  put state {getVarsSet = Set.insert name idents}

-- Init

initSys :: Pred -> StateP Pred
initSys sys = do
  state <- get
  put $ state {getVarsSet = Set.empty}

  sys <- return $ Pnot sys

  -- 1. Eliminate biconditionals and implications
  sys <- return $ elimBci sys

  -- 2. Move negation inwards
  sys <- return $ moveNeg sys

  -- 3. Standardize variables apart by renaming them
  sys <- standardizeIdents sys

  -- 4. Skolemize
  sys <- skolemize sys

  -- 5. Drop universal quantifiers
  sys <- return $ removeUni sys

  -- 6. Distribute conjuction over disjunction
  sys <- return $ distribConj sys

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
standardizeIdents (Or     a b) = liftM2 Or   (standardizeIdents a) (standardizeIdents b)
standardizeIdents (And    a b) = liftM2 And  (standardizeIdents a) (standardizeIdents b)
standardizeIdents (Pnot   a  ) = liftM  Pnot (standardizeIdents a)
standardizeIdents a            = return a

standardizeIdent :: Quantifier -> String -> Pred -> StateP Pred
standardizeIdent f name p = do
  state <- get
  let idents = getVarsSet state

  (name, p) <- if name `elem` idents
    then do
      let name' = getAvailVar' idents name
      let p'    = substIdentP name (ExprI Var name') p
      return (name', p')
    else return (name, p)

  put $ state {getVarsSet = Set.insert name idents}
  p <- standardizeIdents p

  return $ f name p

substIdentP :: String -> Expr -> Pred -> Pred
substIdentP x y (Forall a b) = Forall a $ substQuantifier x y a b
substIdentP x y (Exists a b) = Exists a $ substQuantifier x y a b
substIdentP x y (Or     a b) = substIdentP x y a `Or`  substIdentP x y b
substIdentP x y (And    a b) = substIdentP x y a `And` substIdentP x y b
substIdentP x y (Pnot   a  ) = Pnot $ substIdentP x y a
substIdentP x y (Stat   a  ) = Stat $ substIdentE x y a
substIdentP x y a = error $ show a

substIdentE :: String -> Expr -> Expr -> Expr
substIdentE x y (ExprI t a) = if a == x
  then y
  else ExprI t a
substIdentE x y (ExprP a b) = ExprP (substIdentE x y a) (substIdentE x y b)

substQuantifier :: String -> Expr -> String -> Pred -> Pred
substQuantifier x y a p = if a == x
  then p
  else substIdentP x y p

skolemize :: Pred -> StateP Pred
skolemize = skolemize' Set.empty

skolemize' :: Set String -> Pred -> StateP Pred
skolemize' idents (Forall a b) = liftM (Forall a) $ skolemize' (Set.insert a idents) b
skolemize' idents (Exists a b) = do
  sf <- createSkolemFunc idents
  skolemize' idents $ substIdentP a sf b
skolemize' idents (Or     a b) = liftM2 Or   (skolemize' idents a) (skolemize' idents b)
skolemize' idents (And    a b) = liftM2 And  (skolemize' idents a) (skolemize' idents b)
skolemize' idents (Pnot   a  ) = liftM  Pnot (skolemize' idents a)
skolemize' idents a            = return a

createSkolemFunc :: Set String -> StateP Expr
createSkolemFunc idents = do
  consts <- gets getConstsSet
  let sfName = getAvailConst consts
  addConst sfName
  let sfExpr = ExprI Const sfName
  let exprs = map (ExprI Var) $ Set.toList idents
  return $ foldl ExprP sfExpr exprs

removeUni :: Pred -> Pred
removeUni (Forall a b) = removeUni b
removeUni (Or     a b) = removeUni a `Or`  removeUni b
removeUni (And    a b) = removeUni a `And` removeUni b
removeUni (Pnot   a  ) = Pnot $ removeUni a
removeUni a            = a

distribConj :: Pred -> Pred
distribConj (Or  a b) = distribConj' (distribConj a) (distribConj b)
distribConj (And a b) = distribConj a `And` distribConj b
distribConj a         = a

distribConj' :: Pred -> Pred -> Pred
distribConj' (And a b) c = distribConj $ (a `Or` c) `And` (b `Or` c)
distribConj' a (And b c) = distribConj $ (a `Or` b) `And` (a `Or` c)
distribConj' a b         = a `Or` b