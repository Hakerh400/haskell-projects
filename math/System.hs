{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module System (
  IdentType(..),
  Ident,
  Pred(..),
  Proof,

  ruleMp,
  ruleUniIntr,
  ruleUniElimp,
  ruleUniElims,

  axProp1,
  axProp2,
  axProp3,

  true,
  false,
  lor,
  land,
  iff,
  exi,
  nexi,
  unique,
  eq,
  neq,

  a'p, b'p, c'p,
  d'p, e'p, f'p,

  a's, b's, c's,
  d's, e's, f's,

  a', b', c',
  d', e', f',

  proof2pred,
  getAvailIdent,
  getAvailIdentFromSet,
  ide,
  idep,
  int2identp,
  int2idents,
  ident2int,
  int2identStr,
  ite,
  getFreeIdents,
  hasFreeIdent,
  subst,

  assert,
  assertFail
) where

----------------------------------------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------------------------------------

import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

----------------------------------------------------------------------------------------------------
-- Data constructors
----------------------------------------------------------------------------------------------------

data IdentTypePred = IdentTypePred
data IdentTypeSet = IdentTypeSet

data Ident a =
  IdentFirst |
  IdentNext (Ident a)
  deriving (Eq, Ord)

type IdentP = Ident IdentTypePred
type IdentS = Ident IdentTypeSet

data Pred =
  PredIdent IdentP |
  Impl Pred Pred |
  Not Pred |
  AllP IdentP Pred |
  AllS IdentS Pred |
  In IdentS IdentS
  deriving (Eq, Ord)

data Proof = Proof Pred

instance (IdentPS (Ident a)) => Show (Ident a) where
  show = int2identStr . ident2int

instance Show Pred where
  show (PredIdent a) = show a
  show (Impl a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (Not a) = "~" ++ show a
  show (AllP a b) = "\\" ++ show a ++ " " ++ show b
  show (AllS a b) = "\\" ++ show a ++ " " ++ show b
  show (In a b) = "(" ++ show a ++ " E " ++ show b ++ ")"

instance Show Proof where
  show (Proof a) = "|- " ++ show a

----------------------------------------------------------------------------------------------------
-- Classes
----------------------------------------------------------------------------------------------------

class IdentType a where
instance IdentType IdentTypePred where
instance IdentType IdentTypeSet where

class (Ord a) => IdentOfType a where
instance (IdentType a) => IdentOfType (Ident a) where

class (IdentOfType a) => IdentPS a where
  forall :: a -> Pred -> Pred
  ide :: String -> a
  getFreeIdents :: Pred -> Set a
  uniIntr :: a -> a -> Pred -> Proof -> Proof
  subst :: a -> a -> Pred -> Pred
  getAvailIdentFromSet :: Set a -> a

instance IdentPS IdentP where
  forall = AllP

  ide a = ide' a 0 where
    ide' :: String -> Integer -> IdentP
    ide' str n =
      ite (int2identStr n == str)
      (int2identp n) $
      ide' str (n + 1)

  getFreeIdents (PredIdent a) = Set.singleton a
  getFreeIdents (Impl a b) = getFreeIdents a `Set.union` getFreeIdents b
  getFreeIdents (Not a) = getFreeIdents a
  getFreeIdents (AllP a b) = a `Set.delete` getFreeIdents b
  getFreeIdents (AllS a b) = getFreeIdents b
  getFreeIdents (In a b) = Set.empty

  uniIntr alpha beta phi (Proof (Impl x y)) =
    assert (not $ hasFreeIdent beta x)
    ("uniIntr " ++ "3") $
    ite (alpha == beta)
      result $
      assert (substp alpha betaPred phi == y)
      ("uniIntr " ++ "2") $
      assert (not $ hasFreeIdent beta phi)
      ("uniIntr " ++ "3") $
      result
    where
      betaPred = PredIdent beta
      result = Proof $ Impl x $ forall alpha phi

  subst a b = substp a (PredIdent b)

  getAvailIdentFromSet set = getAvailIdentFromSet' set IdentFirst where
    getAvailIdentFromSet' :: Set IdentP -> IdentP -> IdentP
    getAvailIdentFromSet' set ident =
      ite (ident `Set.member` set)
      ident $
      getAvailIdentFromSet' set $ IdentNext ident

instance IdentPS IdentS where
  forall = AllS

  ide a = ide' a 0 where
    ide' :: String -> Integer -> IdentS
    ide' str n =
      ite (int2identStr n == str)
      (int2idents n) $
      ide' str (n + 1)

  getFreeIdents (PredIdent a) = Set.empty
  getFreeIdents (Impl a b) = getFreeIdents a `Set.union` getFreeIdents b
  getFreeIdents (Not a) = getFreeIdents a
  getFreeIdents (AllP a b) = getFreeIdents b
  getFreeIdents (AllS a b) = a `Set.delete` getFreeIdents b
  getFreeIdents (In a b) = Set.fromList [a, b]

  uniIntr alpha beta phi (Proof (Impl x y)) =
    assert (not $ hasFreeIdent beta x)
    ("uniIntr " ++ "3") $
    ite (alpha == beta)
      result $
      assert (substs alpha beta phi == y)
      ("uniIntr " ++ "2") $
      assert (not $ hasFreeIdent beta phi)
      ("uniIntr " ++ "3") $
      result
    where
      result = Proof $ Impl x $ forall alpha phi

  subst = substs

  getAvailIdentFromSet set = getAvailIdentFromSet' set IdentFirst where
    getAvailIdentFromSet' :: Set IdentS -> IdentS -> IdentS
    getAvailIdentFromSet' set ident =
      ite (ident `Set.member` set)
      ident $
      getAvailIdentFromSet' set $ IdentNext ident

-- instance IdentPS (Ident a) where

----------------------------------------------------------------------------------------------------
-- Inference rules
----------------------------------------------------------------------------------------------------

ruleMp :: Proof -> Proof -> Proof
ruleMp (Proof x@(Impl a b)) (Proof c) =
  assert (a == c)
  ("Modus ponens cannot be applied on `" ++ show x ++ "` `" ++ show c ++ "`, because `" ++ show a ++ "` is not equal to `" ++ show c ++ "`") $
  Proof b

ruleUniIntr :: (IdentPS a) => a -> a -> Pred -> Proof -> Proof
ruleUniIntr = uniIntr

ruleUniElimp :: Pred -> Proof -> Proof
ruleUniElimp beta (Proof (Impl x (AllP alpha phi))) =
  Proof $ Impl x $ substp alpha beta phi

ruleUniElims :: IdentS -> Proof -> Proof
ruleUniElims beta (Proof (Impl x (AllS alpha phi))) =
  Proof $ Impl x $ substs alpha beta phi

----------------------------------------------------------------------------------------------------
-- Propositional logic
----------------------------------------------------------------------------------------------------

axProp1 :: Pred -> Pred -> Proof
axProp1 a b = Proof $ Impl a $ Impl b a

axProp2 :: Pred -> Pred -> Pred -> Proof
axProp2 a b c = Proof $ Impl (Impl a $ Impl b c) $ Impl (Impl a b) $ Impl a c

axProp3 :: Pred -> Pred -> Proof
axProp3 a b = Proof $ Impl (Impl (Not a) (Not b)) $ Impl b a

----------------------------------------------------------------------------------------------------
-- Syntax definitions
----------------------------------------------------------------------------------------------------

true :: Pred
true = AllP a'p $ Impl a' a'

false :: Pred
false = Not true

lor :: Pred -> Pred -> Pred
lor a b = Impl (Not a) b

land :: Pred -> Pred -> Pred
land a b = Not $ lor (Not a) (Not b)

iff :: Pred -> Pred -> Pred
iff a b = land (Impl a b) (Impl b a)

exi :: (IdentPS a) => a -> Pred -> Pred
exi a phi = Not $ forall a $ Not phi

nexi :: (IdentPS a) => a -> Pred -> Pred
nexi a phi = Not $ exi a phi

unique :: IdentS -> Pred -> Pred
unique a phi = land (exi a phi) $
  forall a $ forall b $
  Impl (land phi (subst a b phi)) (eq a b)
  where
    b = getAvailIdent phi

eq :: IdentS -> IdentS -> Pred
eq a b = land
  (AllS c $ iff (In c a) (In c b))
  (AllS c $ iff (In a c) (In b c))
  where
    c = getAvailIdentFromSet $ Set.fromList [a, b]

neq :: IdentS -> IdentS -> Pred
neq a b = Not $ eq a b

----------------------------------------------------------------------------------------------------
-- Identifiers
----------------------------------------------------------------------------------------------------

a'p = (ide "a" :: IdentP); b'p = (ide "b" :: IdentP); c'p = (ide "c" :: IdentP);
d'p = (ide "d" :: IdentP); e'p = (ide "e" :: IdentP); f'p = (ide "f" :: IdentP);

a's = (ide "a" :: IdentS); b's = (ide "b" :: IdentS); c's = (ide "c" :: IdentS);
d's = (ide "d" :: IdentS); e's = (ide "e" :: IdentS); f's = (ide "f" :: IdentS);

a' = idep "a"; b' = idep "b"; c' = idep "c";
d' = idep "d"; e' = idep "e"; f' = idep "f";

----------------------------------------------------------------------------------------------------
-- Other functions
----------------------------------------------------------------------------------------------------

ident2int :: (IdentPS (Ident a)) => Ident a -> Integer
ident2int IdentFirst = 0
ident2int (IdentNext a) = 1 + (ident2int a)

int2identp :: Integer -> IdentP
int2identp 0 = IdentFirst
int2identp n = IdentNext $ int2identp (n - 1)

int2idents :: Integer -> IdentS
int2idents 0 = IdentFirst
int2idents n = IdentNext $ int2idents (n - 1)

idep :: String -> Pred
idep = PredIdent . ide

int2identStr :: Integer -> String
int2identStr n = int2identStr' (n + 1) where
  int2identStr' :: Integer -> String
  int2identStr' 0 = []
  int2identStr' n = rest ++ [letter] where
    letter = chr (97 + (fromIntegral $ (n - 1) `mod` 26))
    rest = int2identStr' ((n - 1) `div` 26)

proof2pred :: Proof -> Pred
proof2pred (Proof a) = a

getAvailIdent :: (IdentPS a) => Pred -> a
getAvailIdent expr = getAvailIdentFromSet $ getFreeIdents expr

ite :: Bool -> a -> a -> a
ite a b c = if a then b else c

hasFreeIdent :: (IdentPS a) => a -> Pred -> Bool
hasFreeIdent a b = a `Set.member` getFreeIdents b

substp :: IdentP -> Pred -> Pred -> Pred
substp x y expr = substp' x y (getFreeIdents y) Set.empty expr where
  substp' :: IdentP -> Pred -> Set IdentP -> Set IdentP -> Pred -> Pred
  substp' x y freeIdents idents z = case z of
    z@(PredIdent a) -> ite (a /= x) z replace
    (Impl a b) -> Impl (s idents a) (s idents b)
    (Not a) -> Not (s idents a)
    z@(AllP a b) -> ite (a == x) z $
      AllP a (s (a `Set.insert` idents) b)
    z@(AllS a b) -> AllS a (s idents b)
    z@(In a b) -> z
    where
      s :: Set IdentP -> Pred -> Pred
      s = substp' x y freeIdents
      replace :: Pred
      replace = 
        assert (freeIdents `Set.intersection` idents == Set.empty)
        ("No free occurrence of `" ++ show x ++ "` in `" ++ show z ++ "` may fall within the scope of a quantifier quantifying a variable occurring in `" ++ show y ++ "`")
        y

substs :: IdentS -> IdentS -> Pred -> Pred
substs x y expr = substs' x y Set.empty expr where
  substs' :: IdentS -> IdentS -> Set IdentS -> Pred -> Pred
  substs' x y idents z = case z of
    z@(PredIdent a) -> z
    (Impl a b) -> Impl (s idents a) (s idents b)
    (Not a) -> Not (s idents a)
    z@(AllP a b) -> AllP a (s idents b)
    z@(AllS a b) -> ite (a == x) z $
      AllS a (s (a `Set.insert` idents) b)
    (In a b) -> In (substIdent a) (substIdent b) where
      substIdent :: IdentS -> IdentS
      substIdent ident = ite (ident /= x) x $
        assert (not $ y `Set.member` idents)
        ("No free occurrence of `" ++ show x ++ "` in `" ++ show z ++ "` may fall within the scope of a quantifier quantifying variable `" ++ show y ++ "`")
        y
    where
      s :: Set IdentS -> Pred -> Pred
      s = substs' x y

assert :: Bool -> String -> a -> a
assert True _ a = a
assert _ msg _ = assertFail msg

assertFail :: String -> a
assertFail msg = error $ "Assertion failed: " ++ msg