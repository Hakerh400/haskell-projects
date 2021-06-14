import Data.Char
import Control.Monad.State

main :: IO ()
main = mapM_ putStrLn output

output :: [String]
output = execState (func 9) []

func :: Int -> State [String] ()
func n = do
  lines <- get
  if any (any (== digit2char n)) lines
    then return ()
    else if null lines
      then do
        put ["0"]
        func n
      else do
        let zs = replicate (length lines) '0'
        put $
          map (surround '0') $
          surround zs $
          map (map nextDigit) $
          lines
        func n

digit2char :: Int -> Char
digit2char n = chr (n + 0x30)

char2digit :: Char -> Int
char2digit c = ord c - 0x30

nextDigit :: Char -> Char
nextDigit c = digit2char $ char2digit c + 1

surround :: a -> [a] -> [a]
surround x xs = x : xs ++ [x]