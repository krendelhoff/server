module Test where

import           Relude

class ExpSYM repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr

instance ExpSYM Int where
  lit = id
  neg n = -n
  add = (+)

x :: ExpSYM repr => repr
x = add (lit 8) (neg (add (lit 1) (lit 2)))

filtr' :: ((a -> Bool) -> [a] -> [a]) -> ((a -> Bool) -> [a] -> [a])
filtr' self p =
  foldr
    (\x acc ->
       if p x
         then x : acc
         else acc)
    []

filtr = fix filtr'
