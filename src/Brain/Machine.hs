module Brain.Machine where

import Brain.Type

data Machine a = Machine {
  former :: [a]
, value :: a
, latter :: [a]
}

newMachine :: (Num a, Enum a) => Machine a
newMachine = Machine [] 0 [0, 0..]

incPoint, decPoint :: Machine a -> Machine a
incPoint (Machine b v a) = let x:xs = a in Machine (v:b) x xs
decPoint (Machine b v a) = let x:xs = b in Machine xs x (v:a)

incValue, decValue :: (Enum a) => Machine a -> Machine a
incValue = modifyValue succ
decValue = modifyValue pred

modifyValue :: (a -> a) -> Machine a -> Machine a
modifyValue f (Machine b v a) = Machine b (f v) a
