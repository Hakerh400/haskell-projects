{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.List

main :: IO ()
main = print (proofEqInherit (2 :: Integer) 2)

----------------------------------------------------------------------------------------------------

class Exa a where
  exaT :: a -> String
  exa :: a -> String

instance Exa Integer where
  exaT _ = "Integer"
  exa a = show a

----------------------------------------------------------------------------------------------------

data Pred :: * where
  Equals :: (Exa a) => a -> a -> Pred
  Not :: Pred -> Pred
  Impl :: Pred -> Pred -> Pred
  Or :: Pred -> Pred -> Pred
  And :: Pred -> Pred -> Pred
  Forall :: Pred -> Pred -> Pred

instance Exa Pred where
  exaT _ = "Pred"
  exa (Equals a b) = exaOp "=" a b
  exa (Not a) = "~" ++ exa a
  exa (Impl a b) = exaOp "->" a b
  exa (Or a b) = exaOp "||" a b
  exa (And a b) = exaOp "&&" a b

instance Show Pred where
  show = exa

----------------------------------------------------------------------------------------------------

data Proof = Proof Pred

instance Exa Proof where
  exaT _ = "Proof"
  exa (Proof a) = "|- " ++ exa a

instance Show Proof where
  show = exa

proofEqInherit :: (Exa a, Eq a) => a -> a -> Proof
proofEqInherit a b = if a == b
  then Proof (Equals a b)
  else error $ concat ["Value ", sf a, " is not equal to ", sf b]

----------------------------------------------------------------------------------------------------

exaOp :: (Exa a, Exa b) => String -> a -> b -> String
exaOp op a b = concat ["(", exa a, " ", op, " ", exa b, ")"]

sf :: (Exa a) => a -> String
sf a = "\"" ++ exa a ++ "\""