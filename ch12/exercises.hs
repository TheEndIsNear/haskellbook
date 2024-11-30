module Exercises where

import Data.Maybe (fromMaybe)

-- 1.
-- Given id :: a -> a
-- What is the kind of a?
--
-- 2.
-- r :: a -> f a
-- what are the kinds of a and f?
--
-- Write a function named replaceThe that takes a text/string an breaks it into words and repaces eache instance of "the" with "a".
--
-- replaceThe :: String -> String
-- replaceThe "the cow loves us" "a cow loves us"
replaceThe :: String -> String
replaceThe str =
  let w = words str
      notThe str
        | str == "the" = Nothing
        | otherwise = Just str
      maybeWords = map notThe w
      replace s = fromMaybe "a" s
   in unwords $ map replace maybeWords

vowels :: [Char]
vowels = ['a', 'e', 'i', 'o', 'u']

consonants :: [Char]
consonants =
  [ 'b'
  , 'c'
  , 'd'
  , 'f'
  , 'g'
  , 'h'
  , 'j'
  , 'k'
  , 'l'
  , 'm'
  , 'n'
  , 'p'
  , 'q'
  , 'r'
  , 's'
  , 't'
  , 'v'
  , 'w'
  , 'x'
  , 'y'
  , 'z'
  ]

isVowel :: Char -> Bool
isVowel c = c `elem` vowels

isConsonant :: Char -> Bool
isConsonant c = c `elem` consonants

countVowels :: String -> Int
countVowels str = length $ filter id $ map isVowel str

countConsonants :: String -> Int
countConsonants str = length $ filter id $ map isConsonant str

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str = check
  where
    cCount = countConsonants str
    vCount = countVowels str
    check =
      if vCount > cCount
        then Nothing
        else Just (Word' str)
