{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}

import Data.Kind

import qualified Prelude as P
import Prelude hiding
  ( and
  , head
  , tail
  , init
  , map
  , length
  , replicate
  , (++)
  , take
  )

type N = Integer

data Nat = Zero | Suc Nat

instance Show Nat where
  show = show . natToInt

infixr 6 +
type (+) :: Nat -> Nat -> Nat
type family a + b where
  Zero + b = b
  Suc a + b = Suc (a + b)

natToInt :: Nat -> N
natToInt Zero = 0
natToInt (Suc n) = 1 + natToInt n

type SNat :: Nat -> Type
data SNat n where
  SZero :: SNat Zero
  SSuc :: SNat n -> SNat (Suc n)

instance Show (SNat n) where
  show = show . snatToNat

snatToNat :: SNat n -> Nat
snatToNat SZero = Zero
snatToNat (SSuc n) = Suc (snatToNat n)

infixr 5 :>
type Vec :: Nat -> Type -> Type
data Vec n a where
  Nil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Suc n) a

instance (Show a) => Show (Vec n a) where
  show = show . toList

toList :: Vec n a -> [a]
toList Nil = []
toList (x:>xs) = x : toList xs

and :: Vec n Bool -> Bool
and Nil = True
and (x:>xs) = x && and xs

head :: Vec (Suc n) a -> a
head (x:>_) = x

tail :: Vec (Suc n) a -> Vec n a
tail (_:>xs) = xs

init :: Vec (Suc n) a -> Vec n a
init (_:>Nil) = Nil
init (x:>ys@(_:>_)) = x :> init ys

map :: (a -> b) -> Vec n a -> Vec n b
map _ Nil = Nil
map f (x:>xs) = f x :> map f xs

length :: Vec n a -> SNat n
length Nil = SZero
length (_:>xs) = SSuc (length xs)

replicate :: SNat n -> a -> Vec n a
replicate SZero _ = Nil
replicate (SSuc n) x = x :> replicate n x

infixr 5 ++
(++) :: Vec n1 a -> Vec n2 a -> Vec (n1 + n2) a
Nil ++ ys = ys
(x:>xs) ++ ys = x :> (xs ++ ys)

-- take :: SNat n1 -> Vec (n2 + n1) a -> Vec n1 a
-- take SZero _ = Nil
-- take (SSuc n) (x:>xs) = x :> take n xs

main :: IO ()
main = do
  print $ (1 :> 2 :> 3 :> Nil) ++ (4 :> 5 :> Nil)