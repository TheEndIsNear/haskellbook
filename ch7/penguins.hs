data WherePenguinsLive
  = Galapagos
  | Antartica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

newtype Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarticPenguin :: Penguin -> Bool
antarticPenguin (Peng Antartica) = True
antarticPenguin _ = False

antarticOrGalapagos :: Penguin -> Bool
antarticOrGalapagos p = galapagosPenguin p || antarticPenguin p
