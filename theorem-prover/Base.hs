{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Base
  ( N
  , join
  , inParens
  , trimLeft
  , trimRight
  , reversed
  , ok
  , err
  ) where

import Data.Char
import Data.List
import Data.Kind

type N = Integer

join :: [a] -> [[a]] -> [a]
join = intercalate

inParens :: String -> String
inParens = ("(" ++) . (++ ")")

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = reversed trimLeft

reversed :: ([a] -> [b]) -> [a] -> [b]
reversed f = reverse . f . reverse

ok :: IO ()
ok = putStrLn "ok"

err :: [String] -> a
err = error . concat
