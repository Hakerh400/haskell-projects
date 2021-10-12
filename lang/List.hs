module List
  ( List
  , nil
  , cons
  , list_exa
  ) where

import qualified Prelude as P

data List a
  = Nil
  | Cons a (List a)

nil :: List a
nil = Nil

cons :: a -> List a -> List a
cons = Cons

list_exa :: (a -> List a -> b -> b) -> b -> List a -> b
list_exa f z Nil = z
list_exa f z (Cons x xs) = f x xs (list_exa f z xs)