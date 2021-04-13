{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

main :: IO ()
main = putStrLn output

output :: String
output = showInfer (th_impl_refl (PredP P))

u :: a
u = undefined

----------------------------------------------------------------------------------------------------

data P :: * where
  P :: P

data Pred :: * -> * where
  PredP :: P -> Pred P
  PredImpl :: Impl a b -> Pred (Impl a b)

data Impl :: * -> * -> * where
  Impl :: Pred a -> Pred b -> Impl a b

data Infer :: * -> * where
  MP :: Infer (Impl a b) -> Infer a -> Infer b
  Ax1 :: Pred a -> Pred b -> Infer (Impl a (Impl b a))
  Ax2 :: Pred a -> Pred b -> Pred c -> Infer (Impl (Impl a (Impl b c)) (Impl (Impl a b) (Impl a c)))

----------------------------------------------------------------------------------------------------

showInfer :: Infer a -> String
showInfer (MP a b) = "(MP " ++ (showInfer a) ++ " " ++ (showInfer b) ++ ")"
showInfer (Ax1 a b) = "(Ax1 " ++ (showPred a) ++ " " ++ (showPred b) ++ ")"
showInfer (Ax2 a b c) = "(Ax2 " ++ (showPred a) ++ " " ++ (showPred b) ++ " " ++ (showPred c) ++ ")"

showPred :: Pred a -> String
showPred (PredP a) = showA a
showPred (PredImpl a) = showImpl a

showImpl :: Impl a b -> String
showImpl (Impl a b) = "(" ++ (showPred a) ++ " -> " ++ (showPred b) ++ ")"

showA :: P -> String
showA a = "P"

----------------------------------------------------------------------------------------------------

type T1 a = Impl a a
type T2 a = Impl a (T1 a)
type T3 a = Impl a (Impl (T1 a) a)

th_impl_refl :: forall a. Pred a -> Infer (Impl a a)
th_impl_refl x = step_6 where
  step_1 = PredImpl (Impl x x) :: Pred (T1 a)
  step_2 = Ax1 x x :: Infer (T2 a)
  step_3 = Ax1 x step_1 :: Infer (T3 a)
  step_4 = Ax2 x step_1 x :: Infer (Impl (T3 a) (Impl (T2 a) (T1 a)))
  step_5 = MP step_4 step_3 :: Infer (Impl (T2 a) (T1 a))
  step_6 = MP step_5 step_2 :: Infer (T1 a)
