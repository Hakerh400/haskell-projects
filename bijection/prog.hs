{-# LANGUAGE NoImplicitPrelude #-}
module Prog where

data E = T | P E E

main a = f a T

f T a = a
f (P a b) c = f b (P a c)