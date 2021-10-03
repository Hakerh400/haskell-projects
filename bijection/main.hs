import Data.List
import Data.Maybe
import Data.Foldable
import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Base
import Tree
import TreeSer
import Expr
import ProgInfo
import Parser

type Defs = Map String Tree

data ITree
  = IIdent String
  | ITree Tree
  | ICall ITree ITree
  deriving (Eq, Show)

srcFile = "src.txt"
inpFile = "input.txt"

combI :: ITree
combI = ITree $ Leaf 0

combK :: ITree
combK = ITree $
  Node (Leaf 1) (Node (Leaf 0) (Leaf 0))

combS :: ITree
combS = ITree $ Leaf 1

combD :: ITree
combD = ITree $
  undefined

combF :: ITree
combF = ITree $
  undefined

main :: IO ()
main = do
  let p = putStrLn . concat
  
  src <- readFile srcFile
  -- input <- readFile inpFile
  
  let parsedDefs = parse src
  let defs = foldl processDef initDefs parsedDefs
  let mainDef = (Map.!) defs "main"
  
  p [show mainDef]
  
  putStr ""

initDefs :: Defs
initDefs = Map.fromList
  [ ("nil", Leaf 0)
  , ("pair", Leaf 2)
  , ("exa", Leaf 3)
  ]

processDef :: Defs -> IdentDef -> Defs
processDef defs def = let
  name = _name def
  args = _args def
  pexpr = _expr def
  expr = funcToComb defs args pexpr
  in if name `Map.member` defs
    then error $ concat ["Duplicate definition for ", show name]
    else Map.insert name expr defs

funcToComb :: Defs -> [String] -> ParsedExpr -> Tree
funcToComb defs args pexpr = let
  itreeInit = pexprToItree pexpr
  itree = foldr (processArg defs) itreeInit args
  in itreeToCtree defs itree

processArg :: Defs -> String -> ITree -> ITree
processArg defs arg itree
  | itree == IIdent arg = combI
  | IIdent _ <- itree = ICall combK itree
  | ITree _ <- itree = ICall combK itree
  | ICall left right <- itree = processArgCall arg itree

processArgCall :: String -> ITree -> ITree
processArgCall arg itree@(ICall left right)
  = case (hasLeft, hasRight) of
    (False, False) -> ICall combK itree
    (False, True) -> if right == IIdent arg
      then itree
      else ICall (ICall combD left) pright
    (True, False) -> ICall (ICall combF pleft) right
    (True, True) -> ICall (ICall combS pleft) pright
  where
    hasLeft = hasArg arg left
    hasRight = hasArg arg right
    pleft = processArgCall arg left
    pright = processArgCall arg right

hasArg :: String -> ITree -> Bool
hasArg arg (IIdent name) = name == arg
hasArg arg (ITree _) = False
hasArg arg (ICall left right)
  = hasArg arg left || hasArg arg right

pexprToItree :: ParsedExpr -> ITree
pexprToItree (Ident name) = IIdent name
pexprToItree (Call target arg)
  = ICall (pexprToItree target) (pexprToItree arg)

itreeToCtree :: Defs -> ITree -> Tree
itreeToCtree defs (IIdent name)
  = case Map.lookup name defs of
    Just tree -> tree
    Nothing -> error $ concat ["Undefined identifier ", show name]
itreeToCtree defs (ITree tree) = tree
itreeToCtree defs (ICall target arg)
  = Node (itreeToCtree defs target) (itreeToCtree defs arg)

  -- 
  -- let info = ProgInfo {maxRecDepth = 2}
  -- let depth = 0
  -- 
  -- let i = ExprFunc S [ExprFunc K [], ExprFunc K []]
  -- let arg = ExprTree (Node (Leaf 0) (Leaf 0))
  -- let result = callExpr info depth i arg
  -- 
  -- p [show $ exprToTree result]
  
  -- flip mapM_ (zip [10 ^ 4] [0..]) $ \(n, i) -> do
  --   if i /= 0
  --     then p []
  --     else pure ()
  -- 
  --   let expr = ExprNat n
  -- 
  --   let nat = exprToNat expr
  --   let tree = exprToTree expr
  --   let combArgs = exprToFunc expr
  -- 
  --   let exprNat = ExprNat nat
  --   let exprTree = ExprTree tree
  --   let exprFunc = combArgsToExpr combArgs
  -- 
  --   p [show n]
  --   p ["nat:  ", show $ exprNat]
  --   p ["tree: ", show $ exprTree]
  --   p ["func: ", show $ exprFunc]
  -- 
  --   True <- pure $ exprToNat exprNat == n
  --   True <- pure $ exprToNat exprTree == n
  --   True <- pure $ exprToNat exprFunc == n
  -- 
  --   True <- pure $ exprToTree exprNat == tree
  --   True <- pure $ exprToTree exprTree == tree
  --   True <- pure $ exprToTree exprFunc == tree
  -- 
  --   True <- pure $ exprToFunc exprNat == combArgs
  --   True <- pure $ exprToFunc exprTree == combArgs
  --   True <- pure $ exprToFunc exprFunc == combArgs
  -- 
  --   p ["ok"]
  -- 
  -- putStr []

-- k = 3
-- main :: IO ()
-- main = mapM_ func [0..100] where
--   func n = do
--     let t = deser k n
--     let n1 = ser k t
--     putStrLn $ concat [show n, " -> ", show t]
--     if n1 == n
--       then pure ()
--       else error $ show n1