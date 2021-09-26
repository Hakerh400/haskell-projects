module Maybe
  ( Maybe
  , nothing
  , just
  , maybe_exa
  )
  where

import qualified Prelude as P

data Maybe a = Nothing
             | Just a

nothing :: Maybe a
nothing = Nothing

just :: a -> Maybe a
just = Just

maybe_exa :: (a -> b) -> b -> Maybe a -> b
maybe_exa f z Nothing  = z
maybe_exa f z (Just a) = f a