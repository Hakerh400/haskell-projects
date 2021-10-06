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

import Data.Kind

data A = A
data B = B
data C = C

type family GetType a where
  GetType String = A
  GetType [a] = B
  GetType a = C

class ShowVal a where
  showVal :: String

instance ShowVal A where
  showVal = "string"

instance ShowVal B where
  showVal = "some list"

instance ShowVal C where
  showVal = "something other"

func :: forall a. (ShowVal (GetType a)) => a -> String
func = const $ showVal @(GetType a)

main :: IO ()
main = do
  print $ func "abc"
  print $ func [True]
  print $ func (id, Just flip)