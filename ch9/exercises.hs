module Exercises where

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
