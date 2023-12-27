newtype Name = Name String deriving Show

newtype Acres =
  Acres Int
  deriving (Show)

-- FormerType is a Sum
data FarmerType
  = DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving (Show)

-- Fatrmer is a plain old product of
-- Name, Acres, and FarmerType
data Farmer =
  Farmer Name Acres FarmerType
  deriving (Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec
  { name :: Name
  , acres :: Acres
  , farmerType :: FarmerType
  }
  deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False

--data Automobile = Null
--                | Car { make :: String
--                      , model :: String
--                      , year :: Integer }
--                deriving (Eq, Show)

-- Split out the record/product
data Car = Car { make :: String
               , model :: String
               , year :: Integer
               } deriving (Eq, Show)

-- The Null is still not great, but
-- we're leaving it in to make a point
data Automobile = Null
                | Automobile Car
                deriving (Eq, Show)

data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

-- 3 + 3
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

-- 3 * 3
quantProd1 :: (Quantum, Quantum)
quantProd1  = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2  = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3  = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4  = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5  = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6  = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7  = (Both, Yes)

quantProd8 :: (Quantum, Quantum)
quantProd8  = (Both, No)

quantProd9 :: (Quantum, Quantum)
quantProd9  = (Both, Both)

-- 3 ^ 3
quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes = Yes
quantFlip1 No = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes = Yes
quantFlip2 No = Yes
quantFlip2 Both = No 

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes = Yes
quantFlip3 No = Yes
quantFlip3 Both = Both 

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes = Yes
quantFlip4 No = No
quantFlip4 Both = Yes 

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes = Yes
quantFlip5 No = Both
quantFlip5 Both = Yes 

convert :: Quantum -> Bool
convert Yes = True
convert No = True
convert Both = True

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = False


convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = True
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False
