module Identity
  ( Identity
  , identity
  , identity_exa
  ) where

import qualified Prelude as P

data Identity a
  = Identity a

identity :: a -> Identity a
identity = Identity

identity_exa :: (a -> b) -> Identity a -> b
identity_exa f (Identity a) = f a