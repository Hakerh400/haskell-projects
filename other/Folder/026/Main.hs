{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
import Data.Kind
type family A(a::b)
type T(a::(*)->A(*)-> *)=a(A*)
main=undefined$1::T(->)A