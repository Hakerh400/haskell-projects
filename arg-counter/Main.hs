{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind

type N = Integer

data Func (a :: Type)
data NonFunc

type family GetType a where
  GetType (a -> b) = Func (GetType b)
  GetType a = NonFunc

class ArgCounter a where
  cnt :: N

instance ArgCounter a => ArgCounter (Func a) where
  cnt = 1 + cnt @a

instance ArgCounter NonFunc where
  cnt = 0

countArgs :: forall a. (ArgCounter (GetType a)) => a -> N
countArgs a = cnt @(GetType a)

main :: IO ()
main = do
  print $ countArgs $ "abc"
  print $ countArgs $ not
  print $ countArgs $ const True
  print $ countArgs $ ((+) :: N -> N -> N)
  print $ countArgs $ \a b c -> []

u :: a
u = undefined