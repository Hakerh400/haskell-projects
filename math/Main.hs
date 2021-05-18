{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Char
import Data.List

main :: IO ()
main = print $
  Forall (Wrap "a" :: Wrapper Integer) (\a -> Equals a a)

----------------------------------------------------------------------------------------------------

data Proof = Proof Pred

instance Show Proof where
  show (Proof a) = "|- " ++ show a

proofEqInherit :: (Exa a, Eq a) => a -> a -> Proof
proofEqInherit a b = if a == b
  then Proof (Equals (Val a) (Val b))
  else error $ concat ["Value ", sf a, " is not equal to ", sf b]

----------------------------------------------------------------------------------------------------

data Pred :: * where
  Equals :: (Exa a) => (Wrapper a) -> (Wrapper a) -> Pred
  Not :: Pred -> Pred
  Impl :: Pred -> Pred -> Pred
  Or :: Pred -> Pred -> Pred
  And :: Pred -> Pred -> Pred
  Forall :: (Exa a) => (Wrapper a) -> (Wrapper a -> Pred) -> Pred

instance Show Pred where
  show (Equals a b) = showOp "=" a b
  show (Not a) = "~" ++ show a
  show (Impl a b) = showOp "->" a b
  show (Or a b) = showOp "||" a b
  show (And a b) = showOp "&&" a b
  show (Forall w@(Wrap s) f) = concat ["V", s, ".", exaT (getWrapperType w), " ", show (f w)]

----------------------------------------------------------------------------------------------------

class Exa a where
  exaT :: a -> String
  exa :: a -> String

instance Exa Integer where
  exaT _ = "Integer"
  exa = show

instance (Exa a) => Exa [a] where
  exaT a = if elemType == "Char"
    then "String"
    else concat ["[", elemType, "]"]
    where
      elemType = exaT (getListElemType a)

  exa a = if exaT a == "String"
    then concatMap exa a
    else concat ["[", intercalate ", " (fmap exa a), "]"]

instance Exa Char where
  exaT _ = "Char"
  exa a = [a]

----------------------------------------------------------------------------------------------------

data Wrapper :: * -> * where
  Wrap :: (Exa a) => String -> Wrapper a
  Val :: (Exa a) => a -> Wrapper a

instance (Exa a) => Show (Wrapper a) where
  show (Wrap a) = a
  show (Val a) = exa a

----------------------------------------------------------------------------------------------------

showOp :: (Show a, Show b) => String -> a -> b -> String
showOp op a b = concat ["(", show a, " ", op, " ", show b, ")"]

sf :: (Exa a) => a -> String
sf a = concat ["\"", exa a, "\""]

getListElemType :: [a] -> a
getListElemType = u

getWrapperType :: (Exa a) => Wrapper a -> a
getWrapperType = u

u :: a
u = undefined