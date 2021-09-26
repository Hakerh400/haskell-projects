module Bool
  ( Bool
  , false
  , true
  , bool_exa
  )
  where

import qualified Prelude as P

import Base

newtype Bool = Bool Base

false :: Bool
false = Bool base_leaf

true :: Bool
true = Bool (base_node base_leaf base_leaf)

bool_exa :: a -> a -> Bool -> a
bool_exa t f (Bool b) = base_exa (\_ _ _ _ -> t) f b