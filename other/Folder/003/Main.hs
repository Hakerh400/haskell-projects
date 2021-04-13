{-# LANGUAGE GADTs, ScopedTypeVariables #-}

import qualified Program as P
import CustomList

main :: IO ()
main = putStrLn $ toStr P.output