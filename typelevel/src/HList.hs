module HList where

import           Data.Kind (Constraint, Type)
import qualified Data.Text as T
import           Relude    hiding (All)
import           TextShow

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

showBool :: HList [_1, Bool, _2] -> String
showBool (_ :# b :# _ :# _) = show b

instance All Eq ts => Eq (HList ts) where
  HNil == HNil               = True
  (t1 :# ts1) == (t2 :# ts2) = t1 == t2 && ts1 == ts2

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  HNil <= HNil           = True
  (a :# as) <= (b :# bs) = a <= b && as <= bs

instance All TextShow ts => TextShow (HList ts) where
  showb HNil        = "[]"
  showb (t :# HNil) = "[" <> showb t <> "]"
  showb (t :# ts)   = "[" <> showb t <> "," <> (fromText . T.tail . showt $ ts)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All _ '[] = ()
  All c (t ': ts) = (c t, All c ts)

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5
