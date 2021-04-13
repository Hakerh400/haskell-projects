{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = putStrLn output

output :: String
output = "ok"

axiom :: a
axiom = axiom

----------------------------------------------------------------------------------------------------

data Impl a b = Impl a b
data Not a = Not a

----------------------------------------------------------------------------------------------------

class Pred a where
instance (Pred a, Pred b) => Pred (Impl a b) where
instance (Pred a) => Pred (Not a) where

----------------------------------------------------------------------------------------------------

mp :: (Pred a, Pred b) => Impl a b -> a -> b
mp = axiom

----------------------------------------------------------------------------------------------------

ax1 :: (Pred a, Pred b) => Impl a (Impl b a)
ax1 = axiom
ax2 :: (Pred a, Pred b, Pred c) => Impl (Impl a (Impl b c)) (Impl (Impl a b) (Impl a c))
ax2 = axiom
ax3 :: (Pred a, Pred b) => Impl (Impl (Not a) (Not b)) (Impl b a)
ax3 = axiom

----------------------------------------------------------------------------------------------------

th_impl_refl :: forall a. (Pred a) => Impl a a
th_impl_refl = step_5 where
  step_1 = ax1 :: Impl a (Impl a a)
  step_2 = ax1 :: Impl a (Impl (Impl a a) a)
  step_3 = ax2 :: Impl (Impl a (Impl (Impl a a) a)) (Impl (Impl a (Impl a a)) (Impl a a))
  step_4 = mp step_3 step_2 :: Impl (Impl a (Impl a a)) (Impl a a)
  step_5 = mp step_4 step_1 :: Impl a a