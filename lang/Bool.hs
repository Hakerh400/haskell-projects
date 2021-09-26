module Bool
  ( Bool
  , bool_false
  , bool_true
  , bool_exa
  )
  where

import qualified Prelude as P

data Bool = False
          | True

bool_false :: Bool
bool_false = False

bool_true :: Bool
bool_true = True

bool_exa :: a -> a -> Bool -> a
bool_exa t f True  = t
bool_exa t f False = f