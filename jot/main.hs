import Data.Bits

data Expr =
  K | S |
  Call Expr Expr
  deriving Show

main :: IO ()
main = putStrLn output

output :: String
output = show $ eval (parse prog)

prog :: String
prog = "11111000"

parse :: String -> Expr
parse = foldl parseBit identity where
  parseBit :: Expr -> Char -> Expr
  parseBit expr '0' = Call (Call expr S) K
  parseBit expr '1' = Call S (Call K expr)

identity :: Expr
identity = Call (Call S K) K

eval :: Expr -> Expr
eval K = K
eval S = S
eval (Call a b) = call (eval a) b where
  call :: Expr -> Expr -> Expr
  call (Call K a) b = eval a
  call (Call (Call S a) b) c = eval (Call (Call a c) (Call b c))
  call a b = Call a b