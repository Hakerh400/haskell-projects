import qualified Parser
import qualified Lisp as L
import Types
import Error
import Util

type M = Either Error

type Program = String

srcDir :: IO String
srcDir = joinPth cwd "src"

sysFile :: IO String
sysFile = joinPth srcDir "system.txt"

main :: IO ()
main = do
  srcDirPth <- srcDir
  filePth <- sysFile

  let file = drop (length srcDirPth + 1) filePth
  src <- readFile filePth

  case parseAndMakeProg file src of
    Left  err  -> putStrLn $ show err
    Right prog -> putStrLn prog

  return ()

parseAndMakeProg :: String -> String -> M Program
parseAndMakeProg file src = do
  parsed <- Parser.parse file src
  prog <- makeProg parsed
  return prog

makeProg :: Node -> M Program
makeProg node = do
  uni <- L.uni node

  a <- L.last uni
  
  return $ show a