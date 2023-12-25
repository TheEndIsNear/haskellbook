--data FlowerType = Gardenis
--                | Rose
--                | Lilac
--                deriving Show
type Gardener = String

--data Garden =
--  Garden Gardener FlowerType
--  deriving Show
data Garden
  = Gardenis Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving (Show)

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

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = 
  [ GnuPlusLinux
  ,  OpenBSDNevermindJustBSDStill
  ,  Mac
  ,  Windows
  ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = os, lang = progLang} | os <- allOperatingSystems, progLang <- allLanguages]
