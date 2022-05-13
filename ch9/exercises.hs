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
