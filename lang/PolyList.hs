{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module PolyList
  ( PolyList(..)
  , (#)
  ) where

data PolyList a where
  PNil  :: PolyList '[]
  PCons :: a -> PolyList b -> PolyList (a ': b)

(#) :: a -> PolyList b -> PolyList (a ': b)
(#) = PCons