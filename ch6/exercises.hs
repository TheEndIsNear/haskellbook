module Exercises where

data TisAnInteger =
  TisAn Integer
  deriving (Show)

instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'

data TwoIntegers =
  Two Integer Integer
  deriving (Show)

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt
  = TisAnInt Int
  | TisAString String
  deriving (Show)

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i') = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False

data Pair a =
  Pair a a
  deriving (Show)

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = a == a' && b == b'

data Tuple a b =
  Tuple a b
  deriving (Show)

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a
  = ThisOne a
  | ThatOne a
  deriving (Show)

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

data EitherOr a b
  = Hello a
  | Goodbye b
  deriving (Show)

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False

x :: Int -> Int
x blah = blah + 20

data Person =
  Person Bool
  deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood
  = Blah
  | Woot
  deriving (Show, Eq, Ord)

settleDown :: Mood -> Mood
settleDown x =
  if x == Woot
    then Blah
    else x

type Subject = String

type Verb = String

type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
  Rocks String
  deriving (Eq, Show)

data Yeah =
  Yeah Bool
  deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- Will not typecheck
-- phew = Papu "chases" True
truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- Will not typecheck because we aren't deriving Ord
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
-- Type-Kwon-Do
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = (aToB a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB _int a = aToB a + aToB a
