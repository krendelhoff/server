module ExT where

import           Data.Typeable
import           Relude
import           TextShow

data Boob (a :: k1) (b :: k2)

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

instance TextShow (Has TextShow) where
  showb = elimHas (\s -> "HasShow " <> showb s)

elimHas ::
     (forall a. c a =>
                  a -> r)
  -> Has c
  -> r
elimHas f (Has s) = f s

type Dynamic = Has Typeable

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimHas cast

liftD2 ::
     forall a b r. (Typeable a, Typeable b, Typeable r)
  => Dynamic
  -> Dynamic
  -> (a -> b -> r)
  -> Maybe Dynamic
liftD2 d1 d2 f = fmap Has . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "bad types for pyPlus") $
  asum
    [ liftD2 @String @String a b (++)
    , liftD2 @Int @Int a b (+)
    , liftD2 @String @Int a b $ \strA intB -> strA ++ show intB
    , liftD2 @Int @String a b $ \intA strB -> show intA ++ strB
    , liftD2 @Int @Text a b $ \intA txtB -> showt intA <> txtB
    ]
