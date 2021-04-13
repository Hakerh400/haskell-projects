{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

main :: IO ()
main = putStrLn output

output :: String
output = show (th_3 (PredP P))

u :: a
u = undefined

----------------------------------------------------------------------------------------------------

data Pred :: * -> * where
  PredP :: P -> Pred P
  PredImpl :: Impl a b -> Pred (Impl a b)
  PredNot :: Not a -> Pred (Not a)

data P :: * where
  P :: P

data Impl :: * -> * -> * where
  Impl :: Pred a -> Pred b -> Impl a b

data Not :: * -> * where
  Not :: Pred a -> Not a

data Infer :: * -> * where
  Ax1 :: Pred a -> Pred b -> Infer (Impl a (Impl b a))
  Ax2 :: Pred a -> Pred b -> Pred c -> Infer (Impl (Impl a (Impl b c)) (Impl (Impl a b) (Impl a c)))
  Ax3 :: Pred a -> Pred b -> Infer (Impl (Impl (Not a) (Not b)) (Impl b a))
  MP :: Infer (Impl a b) -> Infer a -> Infer b

----------------------------------------------------------------------------------------------------

instance Show (Pred a) where
  show (PredP a) = show a
  show (PredImpl a) = show a
  show (PredNot a) = show a

instance Show P where
  show P = "P"

instance Show (Impl a b) where
  show (Impl a b) = "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"

instance Show (Not a) where
  show (Not a) = "~" ++ (show a)

instance Show (Infer a) where
  show (Ax1 a b) = "(Ax1 " ++ (show a) ++ " " ++ (show b) ++ ")"
  show (Ax2 a b c) = "(Ax2 " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ ")"
  show (Ax3 a b) = "(Ax3 " ++ (show a) ++ " " ++ (show b) ++ ")"
  show (MP a b) = "(MP " ++ (show a) ++ " " ++ (show b) ++ ")"

----------------------------------------------------------------------------------------------------

type T1 a = Impl a a
type T2 a = Impl a (T1 a)
type T3 a = Impl a (Impl (T1 a) a)
type T4 a = Impl (T2 a) (T1 a)

th_impl_refl :: forall a. Pred a -> Infer (Impl a a)
th_impl_refl x = step_5 where
  wff_1 = PredImpl (Impl x x) :: Pred (T1 a)
  step_1 = Ax1 x x :: Infer (T2 a)
  step_2 = Ax1 x wff_1 :: Infer (T3 a)
  step_3 = Ax2 x wff_1 x :: Infer (Impl (T3 a) (T4 a))
  step_4 = MP step_3 step_2 :: Infer (T4 a)
  step_5 = MP step_4 step_1 :: Infer (T1 a)

type T5 a = Impl (T6 a) (T9 a)
type T6 a = Not (Not a)
type T7 a = Impl (Not a) (T6 a)
type T8 a = Impl (Impl (T6 a) (T7 a)) (Impl (T6 a) (T10 a))
type T9 a = Impl (T7 a) (T10 a)
type T10 a = Impl (Not a) a

th_2 :: forall a. Pred a -> Infer (Impl (Not (Not a)) (Impl (Not a) a))
th_2 x = step_7 where
  wff_1 = PredNot (Not x) :: Pred (Not a)
  wff_2 = PredNot (Not wff_1) :: Pred (T6 a)
  wff_3 = PredImpl (Impl wff_1 wff_2) :: Pred (T7 a)
  wff_4 = PredImpl (Impl wff_1 x) :: Pred (T10 a)
  wff_5 = PredImpl (Impl wff_3 wff_4) :: Pred (T9 a)
  step_1 = Ax3 x wff_1 :: Infer (T9 a)
  step_2 = Ax1 wff_5 wff_2 :: Infer (Impl (T9 a) (T5 a))
  step_3 = MP step_2 step_1 :: Infer (T5 a)
  step_4 = Ax2 wff_2 wff_3 wff_4 :: Infer (Impl (T5 a) (T8 a))
  step_5 = MP step_4 step_3 :: Infer (T8 a)
  step_6 = Ax1 wff_2 wff_1 :: Infer (Impl (T6 a) (T7 a))
  step_7 = MP step_5 step_6 :: Infer (Impl (T6 a) (T10 a))

data Pred' a b =
  Impl' a b |
  Not' a |
  Arg' (Pred a)

prove_wff :: forall a b c. Pred' a b -> Pred c
prove_wff x = case result of
  P -> PredP result
  Impl _ _ -> PredImpl result
  Not _ -> PredNot result
  where
    result :: c
    result = prove_wff' x
    prove_wff' (Arg' x) = x

th_3 :: forall a. Pred a -> Pred (Impl a (Not a))
th_3 x = prove_wff (Impl' (Arg' x) (Not' (Arg' x)))