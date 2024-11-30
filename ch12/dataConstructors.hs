module DataConstructors where

type Name = String

type Age = Integer

data Person =
  Person Name Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  deriving (Eq, Show)

type ValidatePerson a = Either [PersonInvalid] a

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age =
  if age >= 0 then Right age else Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name =
  if name /= "" then Right name else Left [NameEmpty]

mkPerson'' :: Name -> Age -> ValidatePerson Person
mkPerson'' name age = mPerson (nameOkay name) (ageOkay age)
  where
    mPerson ::
         ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
    mPerson (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
    mPerson (Left badName) (Left badAge) = Left (badName ++ badAge)
    mPerson (Left badName) _ = Left badName
    mPerson _ (Left badAge) = Left badAge
