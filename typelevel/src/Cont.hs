module Cont where

import           Relude

newtype Cont a =
  Cont
    { runCont :: forall r. (a -> r) -> r
    }

instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f (Cont c) = Cont $ \cb -> c $ cb . f
  -- cb . f - это и есть продолжение
  -- \cb -> (\callback -> callback a) (cb . f) ~> \cb -> cb . f $ a ~> \cb -> cb b

instance Applicative Cont where
  pure :: a -> Cont a
  pure a = Cont $ \c -> c a
  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  (Cont cf) <*> (Cont c) = Cont $ \cb -> cb . c $ \a -> cf ($ a)

instance Monad Cont where
  return = pure
  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  (Cont c) >>= mf = c mf
