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

import Data.Kind

data A = A
data B = B
data C = C

type family GetType a where
  GetType String = A
  GetType [a] = B
  GetType a = C

class ShowVal a where
  showVal :: a -> String

instance ShowVal A where
  showVal = const "string"

instance ShowVal B where
  showVal = const "some list"

instance ShowVal C where
  showVal = const "something other"

func :: forall a. (ShowVal (GetType a)) => a -> String
func = const $ (showVal :: GetType a -> String) undefined

main :: IO ()
main = do
  print $ func "abc"
  print $ func [True]
  print $ func (id, Just flip)