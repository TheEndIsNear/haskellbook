module Exercises where

import Data.Char

myWords :: String -> [String]
myWords "" = []
myWords xs =
  let nonSpace = (/=) ' '
      word = takeWhile nonSpace xs
      rest = dropWhile nonSpace xs
      next =
        if null rest
          then []
          else tail rest
   in word : myWords next

myFilter :: String -> [String]
myFilter xs = filter (\x -> x `notElem` ["the", "a", "an"]) $ words xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)

filterUpper :: [Char] -> [Char]
filterUpper = filter isUpper

capitalize :: [Char] -> [Char]
capitalize [] = ""
capitalize (x:xs) = toUpper x : xs

allCaps :: [Char] -> [Char]
allCaps = map toUpper

capLetter :: [Char] -> Char
capLetter = toUpper . head

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs)
  | x = myAnd xs
  | otherwise = False

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
  | x = True
  | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

myAll :: (a -> Bool) -> [a] -> Bool
myAll f xs = myAnd $ map f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem val (x:xs)
  | val == x = True
  | otherwise = myElem val xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' val = myAny (== val)

myReverse :: [Char] -> [Char]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) =
  let squishHelper [] [] acc = reverse acc
      squishHelper [] (x:xs) acc = squishHelper x xs acc
      squishHelper (x:xs) ys acc = squishHelper xs ys (x : acc)
   in squishHelper x xs []

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f xs = squish $ myMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = undefined

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined

myMaximum :: (Ord a) => [a] -> a
myMaximum = undefined

myMinimum :: (Ord a) => [a] -> a
myMinimum = undefined
