module Exercises where

thirdLetter :: [Char] -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

input = "Curry is awesome"

rvrs = drop 9 input ++ take 4 (drop 5 input) ++ take 5 input
