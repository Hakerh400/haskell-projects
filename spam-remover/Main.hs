import Prelude hiding (log)
import Data.List

import qualified CP
import Log

main :: IO ()
main = do
  (exitCode, stdout, stderr) <- execSync
    Nothing
    "C:/Program Files/WinRAR/Rar.exe"
    []
    Nothing

  log(intercalate "\n\n" $ filter (not . ("\x43\x6F\x70\x79\x72\x69\x67\x68\x74" `isInfixOf`)) $ sanll stdout)

  return ()

execSync :: Maybe String -> String -> [String] -> Maybe String ->
  IO (Integer, String, String)
execSync cwdm procName procArgs inputm = do
  resultm <- CP.execSync cwdm procName procArgs inputm
  return $ fromJust resultm

fromJust :: Maybe a -> a
fromJust (Just a) = a

split :: ([a] -> [a] -> Maybe Integer) -> [a] -> [[a]]
split func list = split' func 0 [] list [] [] where
  split' :: ([a] -> [a] -> Maybe Integer) -> Integer -> [a] -> [a] -> [a] -> [[a]] -> [[a]]
  split' func skip prev list sub acc = case list of
    [] -> case func prev list of
      Nothing -> reverse (reverse sub : acc)
      _ -> reverse ([] : reverse sub : acc)
    (x:xs) -> case skip of
      0 -> case func prev list of
        Nothing -> split' func 0 (x : prev) xs (x : sub) acc
        Just len -> case len of
          0 -> split' func 0 (x : prev) xs (x : []) (reverse sub : acc)
          n -> split' func (n - 1) (x : prev) xs [] (reverse sub : acc)
      n -> split' func (n - 1) (x : prev) xs sub acc

sanl :: String -> [String]
sanl = split func where
  func :: String -> String -> Maybe Integer
  func _ ('\r':'\n':_) = Just 2
  func _ ('\r':_) = Just 1
  func _ ('\n':_) = Just 1
  func _ _ = Nothing

sanll :: String -> [String]
sanll = split func where
  func :: String -> String -> Maybe Integer
  func _ ('\r':'\n':'\r':'\n':_) = Just 4
  func _ ('\r':'\n':'\r':_) = Just 3
  func _ ('\r':'\n':'\n':_) = Just 3
  func _ ('\r':'\r':'\n':_) = Just 3
  func _ ('\r':'\r':_) = Just 2
  func _ ('\n':'\r':'\n':_) = Just 3
  func _ ('\n':'\r':_) = Just 2
  func _ ('\n':'\n':_) = Just 2
  func _ _ = Nothing