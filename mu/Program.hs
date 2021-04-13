module Program (
  export
) where

import qualified Prelude as P

import Core

zero = mu f where
  f a = a <= a

one = mu f where
  f a = (a <= a) <= a

impl = mu f where
  f a b c = (a <= b) <= c

not = mu f where
  f a b = (a <= zero) <= b

(||) = mu f where
  f a b c = c == impl (not a) b

(&&) = mu f where
  f a b c = a <= (b <= c)

(==) = mu f where
  f a b c = ((a <= b) && (b <= a)) <= c

(/=) = mu f where
  f a b c = c == not (a == b)

(<) = mu f where
  f a b c = c == ((a <= b) && (a /= b))

inc = mu f where
  f a b = a < b

export = inc (inc (inc zero))