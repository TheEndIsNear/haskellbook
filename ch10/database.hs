import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello World!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  , DbNumber 5008
  , DbDate (UTCTime (fromGregorian 2022 11 23) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = map fetchVal (filter isDbDate db)
  where
    isDbDate (DbDate _) = True
    isDbDate _ = False
    fetchVal (DbDate date) = date

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = map fetchVal (filter isDbNumber db)
  where
    isDbNumber (DbNumber _) = True
    isDbNumber _ = False
    fetchVal (DbNumber date) = date

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = maximum $ filterDbDate db

sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr (+) 0 (filterDbNumber db)

avgDb :: [DatabaseItem] -> Double
avgDb db =  sum / len 
  where sum = fromIntegral $ sumDb db 
        len = fromIntegral $ length $ filterDbNumber db
