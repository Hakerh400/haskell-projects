{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examinable (
  TypeInfo(..),
  Type(..),
  Examinable(..),

  getType,
  getVals,
  ttoStr,
  tname,
  tnames
) where

import Common

data TypeInfo = TypeInfo String [TypeInfo]
  deriving Show

data Type = Type TypeInfo [Type]
  deriving Show

getType :: Type -> TypeInfo
getType (Type a _) = a

getVals :: Type -> [Type]
getVals (Type _ a) = a

ttoStr :: [Type] -> String
ttoStr = concat . tnames

tname :: Type -> String
tname (Type _ [Type (TypeInfo a _) _]) = a
tname (Type (TypeInfo a _) _) = a

tnames :: [Type] -> [String]
tnames = fmap tname

class Examinable a where
  examineType :: a -> TypeInfo
  examineVals :: a -> [Type]

  examine :: a -> Type
  examine a = Type (examineType a) (examineVals a)

instance (Examinable a) => Examinable [a] where
  examineType a = TypeInfo "[]" [examineType $ case a of
      [] -> u
      _ -> head a
    ]
  examineVals a = fmap examine a

instance Examinable Bool where
  examineType a = TypeInfo "Bool" []
  examineVals a = [Type (TypeInfo (ite a "true" "false") []) []]

instance Examinable Char where
  examineType a = TypeInfo "Char" []
  examineVals a = [Type (TypeInfo [a] []) []]

instance Examinable Integer where
  examineType a = TypeInfo "Integer" []
  examineVals a = [Type (TypeInfo (show a) []) []]

instance Examinable () where
  examineType a = TypeInfo "()" []
  examineVals a = []

instance (Examinable a, Examinable b) => Examinable (a, b) where
  examineType (a, b) = TypeInfo "(,)" [examineType a, examineType b]
  examineVals (a, b) = [examine a, examine b]