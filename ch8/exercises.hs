module Exercises where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sumn :: (Ord a, Eq a, Num a) => a -> a
sumn n
  | n < 0 = 0
  | otherwise = go n 1 0
  where
    go n count sum
      | n == count = count + sum
      | otherwise = go n (count + 1) (sum + count)

mult :: Integral a => a -> a -> a
mult 0 _ = 0
mult _ 0 = 0
mult x y = go x y 0
  where
    go x y acc
      | y == 0 = acc
      | otherwise = go x (y - 1) (acc + x)

mc91 :: (Integral a, Eq a, Ord a) => a -> a
mc91 n
  | n <= 100 = mc91 . mc91 . (+ n) $ 11
  | otherwise = n - 10
