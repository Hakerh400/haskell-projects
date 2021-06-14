import qualified System.Directory as Dir

import qualified Lisparser

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

  let sysFileName = drop (length srcDirPth + 1) filePth
  src <- readFile filePth

  case Lisparser.parse src of
    Left err   -> putStrLn $ Lisparser.formatErr sysFileName src err
    Right node -> print node

joinPth :: IO String -> String -> IO String
joinPth dir name = do
  d <- dir
  return $ normPth $ d ++ "/" ++ name

normPth :: String -> String
normPth = map $ \c ->
  if c == '\\'
    then '/'
    else c