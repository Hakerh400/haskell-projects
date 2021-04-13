module Program (
  export
) where

import qualified Prelude as P

import Core

zero = mu f where
  f a = a <= a

one = mu f where
  f a = (a <= a) <= a

not = mu f where
  f a b = (a <= zero) <= b

impl = mu f where
  f a b c = (a <= b) <= c

(||) = mu f where
  f a b c = c == (not a `impl` b)

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

dec = mu f where
  f a b = a == inc b

(+) = mu f where
  f a b c =
    ((b == zero) `impl` (c == a)) &&
    ((b /= zero) `impl` (c == inc (a + dec b)))

(-) = mu f where
  f a b c = (c + b) == a

(*) = mu f where
  f a b c =
    ((b == zero) `impl` (c == zero)) &&
    ((b /= zero) `impl` (c == (a + (a * dec b))))

(/) = mu f where
  f a b c = (c * b) == a

export = (int2nat 6) / (int2nat 3)