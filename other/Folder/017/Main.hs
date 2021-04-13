main :: IO ()
main = putStrLn output

u :: a
u = undefined

----------------------------------------------------------------------------------------------------

data Func = Func (Integer -> (Integer, Func))

func :: Integer -> Func
func x = Func func' where
  func' :: Integer -> (Integer, Func)
  func' y = (z, func z) where
    z :: Integer
    z = x + y

call :: Func -> Integer -> (Integer, Func)
call (Func f) n = f n

base :: Func
base = func 0

output :: String
output = show $ fst $ call (snd $ call base 5) 7