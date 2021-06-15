import qualified System.Directory as Dir

import qualified Parser
import qualified Lisp as L
import Types
import Error

cwd :: IO String
cwd = Dir.getCurrentDirectory

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

  case Parser.parse file src of
    Left err   -> putStrLn $ show err
    Right node -> do
      L.ident node

  pure ()

joinPth :: IO String -> String -> IO String
joinPth dir name = do
  d <- dir
  return $ normPth $ d ++ "/" ++ name

normPth :: String -> String
normPth = map $ \c ->
  if c == '\\'
    then '/'
    else c