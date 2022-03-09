module Exercises where

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _b, c) (d, _e, f) = ((a, d), (c, f))

functionC :: (Ord a) => a -> a -> a
functionC x y =
  case test of
    True -> x
    False -> y
  where
    test = x > y

ifEvenAdd2 :: (Integral a) => a -> a
ifEvenAdd2 n =
  case even n of
    True -> (+) n 2
    False -> n

nums :: (Ord a, Num a) => a -> a
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False

numbers :: (Num a, Eq a, Ord a) => a -> Integer
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x >= 0 = 1

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    (xLast, _) = x `divMod` 10
    d = xLast `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y bool =
  case bool of
    True -> y
    False -> x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y bool
  | bool == False = x
  | otherwise = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

g :: (a -> b) -> (a, c) -> (b, c)
g aToB (x, y) = (aToB x, y)
