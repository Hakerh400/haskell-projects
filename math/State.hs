{-# LANGUAGE FlexibleInstances #-}

module State
  ( StateT(..)
  , evalState
  , execState
  , get
  , gets
  , put
  , either2state
  ) where

import Control.Applicative

import MonadE
import Error

newtype StateT s e a = StateT
  {getState :: s -> Either e (a, s)}

instance Functor (StateT s e) where
  fmap f (StateT st) = StateT $ \s ->
    case st s of
      Left err     -> Left err
      Right (a, s) -> Right (f a, s)

instance Applicative (StateT s e) where
  pure a = StateT $ \s -> Right (a, s)
  liftA2 f (StateT st1) (StateT st2) = StateT $ \s ->
    case st1 s of
      Left err      -> Left err
      Right (a1, s) -> case st2 s of
        Left err      -> Left err
        Right (a2, s) -> Right (f a1 a2, s)

instance Monad (StateT s e) where
  return = pure
  StateT st >>= f = StateT $ \s ->
    case st s of
      Left err     -> Left err
      Right (a, s) -> getState (f a) s

instance MonadFail (StateT s e) where
  fail = error

instance MonadE (StateT s Error) where
  throw = StateT . const . Left

evalState :: StateT s e a -> s -> Either e a
evalState (StateT st) s = case st s of
  Left err     -> Left err
  Right (a, _) -> Right a

execState :: StateT s e a -> s -> Either e s
execState (StateT st) s = case st s of
  Left err     -> Left err
  Right (_, s) -> Right s

get :: StateT s e s
get = StateT $ \s -> Right (s, s)

gets :: (s -> a) -> StateT s e a
gets f = get >>= return . f

put :: s -> StateT s e ()
put s = StateT $ const $ Right ((), s)

either2state :: Either Error a -> StateT s Error a
either2state ei = do
  case ei of
    Left  err -> throw err
    Right val -> return val