import qualified System.Directory as Directory

import qualified Lisparser

cwd :: IO String
cwd = Directory.getCurrentDirectory

srcDir :: IO String
srcDir = joinPth cwd "src"

sysFile :: IO String
sysFile = joinPth srcDir "system.txt"

main :: IO ()
main = do
  src <- sysFile >>= readFile
  let sys = Lisparser.parse src
  print sys

joinPth :: IO String -> String -> IO String
joinPth dir name = do
  d <- dir
  return $ d ++ "/" ++ name