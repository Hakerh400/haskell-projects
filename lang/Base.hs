module Base
  ( Base
  , base_leaf
  , base_node
  , base_exa
  )
  where

import qualified Prelude as P

data Base = Leaf
          | Node Base Base

base_leaf :: Base
base_leaf = Leaf

base_node :: Base -> Base -> Base
base_node = Node

base_exa :: (Base -> Base -> a -> a -> a) -> a -> Base -> a
base_exa f z Leaf       = z
base_exa f z (Node l r) = f l r (base_exa f z l) (base_exa f z r)