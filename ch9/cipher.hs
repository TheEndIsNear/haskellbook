module Cipher where

import Data.Char

shift :: Char -> Int -> Char
shift x num
  | isUpper x = chr $ (ord x - ord 'A' + num) `mod` 26 + ord 'A'
  | otherwise = chr $ (ord x - ord 'a' + num) `mod` 26 + ord 'a'

ceasar :: [Char] -> Int -> [Char]
ceasar xs num = map (`shift` num) xs

unCeasar :: [Char] -> Int -> [Char]
unCeasar xs num = map (`shift` (-num)) xs
