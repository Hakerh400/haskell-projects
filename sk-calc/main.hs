import Prelude hiding (zipWith, flip)
import Data.Bits

data Expr =
  K | S |
  Call Expr Expr
  deriving Show

main :: IO ()
main = putStrLn output

output :: String
output = show $ eval prog

prog :: Expr
prog = Call (Call S K) K

eval :: Expr -> Expr
eval K = K
eval S = S
eval (Call a b) = call (eval a) b where
  call :: Expr -> Expr -> Expr
  call (Call K a) b = eval a
  call (Call (Call S a) b) c = eval (Call (Call a c) (Call b c))
  call a b = Call a b