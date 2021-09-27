module Main where

import           Relude
import           TextShow

-- >>> 5 + 5
-- 10
-- >>> 5 + 5
-- 10
-- prop> \(l::[Int]) -> reverse (reverse l) == l
-- +++ OK, passed 100 tests.
-- >>> :t reverse
-- reverse :: [a] -> [a]
-- >>> reverse [1,2,3]
-- [3,2,1]
newtype Temp (u :: TempUnits) =
  Temp Double

data Lvl
  = None
  | Debug
  | All

type family (a :<= b) :: Bool where
  _ :<= None = True
  All :<= Debug = True

data TempUnits
  = F
  | C

class UnitName u where
  unitName :: Text

instance UnitName F where
  unitName = "F"

instance UnitName C where
  unitName = "C"

instance UnitName u => UnitName (Temp u) where
  unitName = unitName @u

instance UnitName unit => TextShow (Temp unit) where
  showb (Temp t) = showb t <> fromText "°" <> fromText (unitName @unit)

unit ::
     forall u. UnitName u
  => Temp u
  -> Text
unit _ = unitName @u

data family XList a

data instance  XList Bool = XBits Int Int
                          | XBots Bool

{-| Booba

>>> 5 + 8
13
>>> "ahahah" <> "hohoho"
"ahahahhohoho"
>>> 5 + 5
10
>>> 5 - 150
-145
boob> \(l::[Int]) -> reverse (reverse l) == l
Variable not in scope:
  propEvaluation :: ([Int] -> Bool) -> IO String
-}
x = 2 + 5

instance UnitName Temp where
  unitName = "_unspecified unit_"

main :: IO ()
main = pass
