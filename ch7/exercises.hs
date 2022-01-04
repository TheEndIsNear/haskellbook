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