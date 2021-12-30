module Exercises where

x :: Num a => a
x = 5

y :: Num a => a
y = x + 5

w :: Num a => a
w = y * 10

z :: Num a => a -> a
z z = y * 10

f :: Fractional a => a
f = 4 / y

x2 :: [Char]
x2 = "Julie"

y2 :: [Char]
y2 = " <3 "

z2 :: [Char]
z2 = "Haskell"

f2 :: [Char]
f2 = x2 ++ y2 ++ z2

bigNum x = (^) x $ 10  

wahoo = bigNum $ 10

x3 = print
y3 = "woohoo!"
z3 = x3 $ y3

a = (+)
b = 5
c = a b 10
d = a c 200

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y =
    if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, yToZ $ xToY x)

i :: a -> a
i x = x 

c' :: a -> b -> a
c' x _ = x

c'' :: a -> b -> b
c'' _ b = b

r :: [a] -> [a]
r (x:xs) = xs

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

a' :: (a -> c) -> a -> a
a' _ x = x

a'' :: (a -> b) -> a -> b
a'' aToB a = aToB a
