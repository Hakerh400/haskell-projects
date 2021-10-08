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
{-# LANGUAGE MultiParamTypeClasses #-}

module Base
  ( N
  , err
  ) where

import Data.Kind

type N = Integer

data ToStrString
data ToStrOther

type family ToStrGetType a where
  ToStrGetType String = ToStrString
  ToStrGetType a = ToStrOther

class ToStrClass a where
  toStrMethod :: (Show b) => b -> String

instance ToStrClass ToStrString where
  toStrMethod = tail . init . show

instance ToStrClass ToStrOther where
  toStrMethod = show

toStr :: forall a b. (Show a, b ~ ToStrGetType a, ToStrClass b) => a -> String
toStr a = toStrMethod @b a

err :: (Show a, ToStrClass (ToStrGetType a)) => [a] -> b
err xs = error $ xs >>= toStr