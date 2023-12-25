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

--data Fiction = Fiction deriving Show
--data NonFiction = NonFiction deriving Show
--data BookType
--  = FictionalBool Fiction
--  | NonfictionBook NonFiction
--  deriving Show
type AuthorName = String

--data Author = Author (AuthorName, BookType)
data Author
  = Fiction AuthorName
  | NonFiction AuthorName
  deriving (Eq, Show)

data Expr
  = Number Int
  | Add Expr Expr
  | Minus Expr Expr
  | Mult Expr Expr
  | Divide Expr Expr

type Number = Int

type Add = (Expr, Expr)

type Minus = Expr

type Mult = (Expr, Expr)

type Divide = (Expr, Expr)

data GuessWhat =
  ChickenButt
  deriving (Eq, Show)

data Id a =
  MkId a
  deriving (Eq, Show)

data Product a b =
  Product a b
  deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pfirst :: a
  , psecond :: b
  } deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Famrhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

trivialValue :: GuessWhat
trivialValue = ChickenButt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

--data Twitter =
--  Twitter deriving (Eq, Show)
--data AskFm =
--  AskFm deriving (Eq, Show)
data SocialNetwork
  = Twitter
  | AskFm
  deriving (Eq, Show)

type Twitter = String

type AskFm = String

twitter :: Sum Twitter AskFm
twitter = First "Twitter"

askfm :: Sum Twitter AskFm
askfm = First "AskFm"

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct {pfirst = 42, psecond = 0.00001}

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgLang
  } deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer {os = Mac, lang = Haskell}

feelingWizardly :: Programmer
feelingWizardly = Programmer {lang = Agda, os = GnuPlusLinux}
