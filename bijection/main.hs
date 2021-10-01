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
import Comb

main :: IO ()
main = do
  print $ combsTable

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