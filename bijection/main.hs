import Data.List
import Data.Maybe
import Data.Foldable
import Control.Applicative
import Control.Monad
import Control.Monad.State

import Base
import Tree
import TreeSer
import Expr
import ProgInfo
import Parser

srcFile = "src.txt"
inpFile = "input.txt"

main :: IO ()
main = do
  let p = putStrLn . concat
  
  src <- readFile srcFile
  -- input <- readFile inpFile
  
  p [show $ parse src]
  
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