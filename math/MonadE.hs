{-# LANGUAGE FlexibleInstances #-}

module MonadE where

import Error

class (Monad m) => MonadE m where
  throw :: Error -> m a

instance MonadE (Either Error) where
  throw = Left