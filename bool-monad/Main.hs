import Control.Applicative

class BoolM m where
  base    :: m a
  wrap    :: Bool -> m a
  unwrap  :: m a -> Bool
  combine :: m a -> m b -> Bool

adapt :: (BoolM m, BoolM m') => m a -> m' b
adapt = wrap . unwrap

newtype Wrap m a = Wrap (m a)

instance (BoolM m) => BoolM (Wrap m) where
  base = Wrap base
  wrap = Wrap . wrap
  unwrap (Wrap a) = unwrap a
  combine (Wrap a) (Wrap b) = combine a b

instance (BoolM m) => Functor (Wrap m) where
  fmap = const adapt

instance (BoolM m) => Applicative (Wrap m) where
  pure = const base
  liftA2 _ a b = wrap $ combine a b

instance (BoolM m) => Monad (Wrap m) where
  return = pure
  a >>= f = wrap $ combine a $ f undefined

instance (BoolM m) => Show (Wrap m a) where
  show a = show $ unwrap a

newtype Conj a = Conj Bool
newtype Disj a = Disj Bool

instance BoolM Conj where
  base = wrap True
  wrap = Conj
  unwrap (Conj a) = a
  combine a b = unwrap a && unwrap b

instance BoolM Disj where
  base = wrap False
  wrap = Disj
  unwrap (Disj a) = a
  combine a b = unwrap a || unwrap b

main :: IO ()
main = print result

result :: Wrap Conj a
result = do
  wrap True
  adapt $ do
    base :: Wrap Disj a
    wrap False
    wrap True