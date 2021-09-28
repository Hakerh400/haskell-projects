{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind

type family T a b where
  T a b = T (T a b) (T a b)

main = undefined :: T () ()