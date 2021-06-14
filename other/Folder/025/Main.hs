{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Kind
type family A(a::b)
type T(a::(*)-> * -> *)=a(A*)
main=let a=a in print$const 1$(const a::T(->)(A*))a