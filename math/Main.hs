import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe
import Data.Foldable
import Control.Monad
import System.IO
import System.Exit

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Parser
import qualified Lisp as L
import ParserTypes
import Error
import Util
import MonadE
import State
import Predicate
import Expression
import CNF
import Solver
import Avail

type State  s   = StateT s Error
type StateP     = State  InfoP
type M          = Either Error
type Quantifier = String -> Pred -> Pred
type ECNF       = Either String CNF

data InfoP = InfoP
  { getConstsSet  :: Set String
  , getVarsSet    :: Set String
  , getOpenQuants :: Map String Int
  } deriving (Show)

mathDir :: IO String
mathDir = joinPth cwd "math"

main :: IO ()
main = do
  mapM_ (flip hSetBuffering NoBuffering)
    [stdin, stdout, stderr]

  baseFile <- joinPth mathDir "base.txt"
  statFile <- joinPth mathDir "stat.txt"

  baseSrc <- readFile baseFile
  statSrc <- readFile statFile

  statCNF <- ei2io $ parseAndInitCNF "stat" statSrc

  let baseSrc' = concat ["(~ (& ", baseSrc, "))"]
  let statConsts = cnfGetConsts statCNF
  baseCNF <- ei2io $ parseAndInitCNF' statConsts "base" baseSrc'

  let cnf = cnfMerge baseCNF statCNF
  prove cnf

ei2io :: Either Error a -> IO a
ei2io (Left  e) = do
  putStrLn $ show e
  exitFailure
ei2io (Right a) = return a

prove :: CNF -> IO ()
prove cnf = if isCnfProved cnf
  then putStrLn "---\n\nProved!"
  else do
    print cnf

    line <- input
    let ws = words line

    updateCNF cnf $ case line of
      [] -> Left ""
      ('.':src) -> do
          cnf' <- induction src cnf
          return $ cnfMerge cnf cnf'
      _ -> case length ws of
        1 -> do
          i <- str2nat line
          removeClause (dec i) cnf
        4 -> do
          indices <- mapM str2nat ws
          let (i1:j1:i2:j2:_) = map dec indices
          combineClauses i1 j1 i2 j2 cnf
        _ -> Left "Unknown command"

inductionScheme :: Node
inductionScheme = let
  src = "(-> (* 0) (all n (-> (* n) (* (S n)))) (all n (* n)))"
  in fromRight $ Parser.parse "induction-scheme" src

induction :: String -> CNF -> ECNF
induction src cnf =  case Parser.parse "induction-expr" src of
  Left  err -> Left $ show err
  Right sys -> case getElem sys of
    (List (uni:[])) -> do
      sys <- return $ substNodeF (Ident "*") (\node -> substNode (Ident "*") node uni) inductionScheme
      let consts = cnfGetConsts cnf
      case parseAndInitSys' consts "induction" $ concat ["(~ ", tail $ init $ show sys, ")"] of
        Left  err -> Left $ show err
        Right sys -> return $ pred2cnf sys
    _ -> Left "Must have exactly one element"

substNodeF :: Elem -> (Node -> Node) -> Node -> Node
substNodeF a f node = let
  e = getElem node
  d = node {getElem = substElemF a f e}
  in case getElem node of
    List (x:y:[]) -> if getElem x == a
      then f y
      else d
    a -> d

substElemF :: Elem -> (Node -> Node) -> Elem -> Elem
substElemF a f (List nodes) = List $ map (substNodeF a f) nodes
substElemF a f c            = c

substNode :: Elem -> Node -> Node -> Node
substNode a b node = let
  e = getElem node
  in if e == a
    then b
    else node {getElem = substElem a b e}

substElem :: Elem -> Node -> Elem -> Elem
substElem a b (List nodes) = List $ map (substNode a b) nodes
substElem a b c            = c

updateCNF :: CNF -> ECNF -> IO ()
updateCNF _   (Right cnf) = prove cnf
updateCNF cnf (Left  err) = if null err
  then return ()
  else putStrLn (err ++ "\n") >> prove cnf

combineClauses :: Int -> Int -> Int -> Int -> CNF -> ECNF
combineClauses i1 j1 i2 j2 cnf = do
  let clauses = cnf2clauses cnf

  if i1 >= length clauses || i2 >= length clauses
    then rangeError
    else return ()

  let clause1 = clause2set $ clauses !! i1
  let clause2 = clause2set $ clauses !! i2

  if j1 >= Set.size clause1 || j2 >= Set.size clause2
    then rangeError
    else return ()

  let item1 = setGet j1 clause1
  let item2 = setGet j2 clause2

  if getItemSign item1 == getItemSign item2
    then Left "Same sign"
    else return ()

  let expr1 = item2expr item1
  let expr2 = item2expr item2

  let vars1 = clauseGetVars $ Clause clause1
  let vars2 = clauseGetVars $ Clause clause2
  let allVars =  vars1 `Set.union` vars2
  let collisions = filterSet (`elem` vars2) vars1

  let availsSequence = getAvails getAvailVar collisions
  let zippedVars = setAsList' (`zip` availsSequence) collisions

  let clause1' = foldr substZippedItems (Set.toList clause1) zippedVars
  let expr1' = item2expr $ clause1' !! j1

  let lhs = expr1'
  let rhs = expr2
  let eq = makeEq lhs rhs

  case solve $ Set.fromList [eq] of
    Nothing  -> Left "Incompatible"
    Just sol -> do
      let c1 = Set.delete item1 $ Set.fromList clause1'
      let c2 = Set.delete item2 clause2

      let c1' = substIdentClause sol c1
      let c2' = substIdentClause sol c2

      let cNew = Clause $ c1' `Set.union` c2'

      if isClauseTaut cNew
        then Left "Tautology"
        else return ()

      return $ cnfAddClause cNew cnf

removeClause :: Int -> CNF -> ECNF
removeClause i cnf = do
  let clauses = cnf2clauses cnf

  if i >= length clauses
    then rangeError
    else do
      let clause = clauses !! i
      return $ CNF $ filter (/= clause) clauses

input :: IO String
input = do
  putStr "\n>>> "
  line <- getLine
  putStrLn ""
  return line

rangeError :: Either String a
rangeError = Left "Out of range"

parseAndInitCNF :: String -> String -> M CNF
parseAndInitCNF = parseAndInitCNF' Set.empty

parseAndInitCNF' :: Set String -> String -> String -> M CNF
parseAndInitCNF' consts file src = do
  sys <- parseAndInitSys' consts file src
  return $ pred2cnf sys

parseSys :: String -> String -> M Pred
parseSys = parseSys' Set.empty

parseSys' :: Set String -> String -> String -> M Pred
parseSys' consts = parseAndInitSys1 False consts

parseAndInitSys :: String -> String -> M Pred
parseAndInitSys = parseAndInitSys' Set.empty

parseAndInitSys' :: Set String -> String -> String -> M Pred
parseAndInitSys' consts = parseAndInitSys1 True consts

parseAndInitSys1 :: Bool -> Set String -> String -> String -> M Pred
parseAndInitSys1 init' consts file src = do
  let func = parseAndInitSys2 init' file src
  let state = InfoP {
    getVarsSet    = Set.empty,
    getConstsSet  = consts,
    getOpenQuants = Map.empty}
  evalState func state

parseAndInitSys2 :: Bool -> String -> String -> StateP Pred
parseAndInitSys2 init' file src = do
  parsed <- either2state $ Parser.parse file src
  sys <- parseSysRaw parsed
  if init'
    then initSys sys
    else return sys

-- Parse

parseSysRaw :: Node -> StateP Pred
parseSysRaw node = do
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
  openQuant name
  p <- unfoldQuantifierIdents f names sp
  closeQuant name
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
          isq <- isQuantified name
          if isq
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

isQuantified :: String -> StateP Bool
isQuantified name = do
  quants <- gets getOpenQuants
  case Map.lookup name quants of
    Nothing -> return False
    Just n  -> return $ n /= 0

openQuant :: String -> StateP ()
openQuant = updateQuant 1

closeQuant :: String -> StateP ()
closeQuant = updateQuant (-1)

updateQuant :: Int -> String -> StateP ()
updateQuant n name = do
  state <- get
  let quants = getOpenQuants state
  let quants' = Map.insertWith (+) name n quants
  put $ state {getOpenQuants = quants'}

-- Init

initSys :: Pred -> StateP Pred
initSys sys = do
  -- state <- get
  -- put $ state {getVarsSet = Set.empty}

  sys <- return $ Pnot sys

  -- 1. Eliminate biconditionals and implications
  sys <- return $ elimBci sys

  -- 2. Move negation inwards
  sys <- return $ moveNeg sys

  -- a<-get
  -- error$(show$a)++replicate 10'\n'
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
      let name' = getAvailVar idents
      let p'    = substIdentP name (ExprI Var name') p
      return (name', p')
    else return (name, p)

  put $ state {getVarsSet = Set.insert name idents}
  p <- standardizeIdents p

  return $ f name p

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