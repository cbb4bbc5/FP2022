module Ex3 where

import Control.Monad (ap)

data StreamTrans i o a
  = Return a
  | ReadS (Maybe i -> StreamTrans i o a)
  | WriteS o (StreamTrans i o a)

instance Functor (StreamTrans i o a) where
  fmap f (Return a)   = Return $ f a
  fmap f (ReadS cont) = ReadS $ (fmap f) . cont
  fmap f (WriteS o s) = WriteS o $ fmap f s

instance Applicative (StreamTrans i o) where
  pure  = return
  (<*>) = ap

composeMonad :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
composeMonad fa fb a = fa a >>= fb

instance Monad (StreamTrans i o) where
  return a = Return a
  (>>=) (Return a) f   = f a
  (>>=) (ReadS cont) f = ReadS (composeMonad cont f)
  (>>=) (WriteS o s) f = WriteS o (s >>= f)

