data Expr =
  S | K | I |
  Ide String |
  Abs String Expr |
  App Expr Expr

main :: IO ()
main = putStrLn (show output)

input :: Expr
input = Abs "a" (Abs "b" (App (Ide "b") (Ide "a")))

output :: Expr
output = lam2ski input

lam2ski :: Expr -> Expr
lam2ski (Abs a e) = case e of
  Ide b -> if b == a then I else App K (Ide b)
  Abs b c -> lam2ski (Abs a (lam2ski (Abs b c)))
  App e1 e2 -> App (App S (lam2ski (Abs a e1))) (lam2ski (Abs a e2))
  a -> App K a
lam2ski (App e1 e2) = App (lam2ski e1) (lam2ski e2)
lam2ski a = a

instance Show Expr where
  show x = show' x False where
    show' x y = case x of
      S -> "S"
      K -> "K"
      I -> "I"
      (Ide a) -> s a
      (Abs a e) -> t (s a ++ " -> " ++ show e)
      (App e1 e2) -> if y then t r else r where
        r :: String
        r = show e1 ++ show' e2 True
      where
        m :: String -> String -> String -> String
        m a b c = a ++ b ++ c
        t :: String -> String
        t a = m "(" a ")"
        s :: String -> String
        s a = m "[" a "]"