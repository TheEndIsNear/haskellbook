{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

import Data.Int

data PugType =
  PugData

data HuskyType a =
  HuskyData

data DogueDeBordeaux doge =
  DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

-- can not work as the type has been declared as String
--badDoge :: DogueDeBordeaux String
--badDoge = DogueDeBordeaux 10
data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

data Price =
  Price Integer
  deriving (Eq, Show)

data Size =
  Size Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir (Size 150)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

arePlanes :: [Vehicle] -> [Bool]
arePlanes = map isPlane

getManu :: Vehicle -> Manufacturer
getManu (Car manufacturer _) = manufacturer

-- nullary
data Example0 =
  Example0
  deriving (Eq, Show)

-- unary
data Example1 =
  Example1 Int
  deriving (Eq, Show)

-- product of Int and String
data Example2 =
  Example2 Int String
  deriving (Eq, Show)

data MyType =
  MyVal Int
  deriving (Eq, Show)

data Example =
  MakeExample
  deriving (Show)

data Example' =
  MakeExample' Int
  deriving (Show)

newtype Goats =
  Goats Int
  deriving (Eq, Show, TooMany)

newtype Cows =
  Cows Int
  deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany $ (+) x y

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)

--instance TooMany Goats where
--  tooMany (Goats n) = tooMany n
data BigSmall
  = Big Bool
  | Small Bool
  deriving (Eq, Show)

data NumberOrBool
  = Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

data QuantumBool
  = QuantumTrue
  | QuantumFalse
  | QuantumBoth
  deriving (Eq, Show)

data TwoQs =
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Eq, Show)
