{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module UE4 where

------------------------------------------------------------------------------------
-- A1
------------------------------------------------------------------------------------
data Zahl
  = N -- N fuer ‘Null’
  | P Zahl -- P fuer ‘plus eins’
  | M Zahl -- M feur ‘minus eins’

-- Instanz für Eq
instance Eq Zahl where
  N == N = True
  P x == P y = x == y
  M x == M y = x == y
  _ == _ = False

-- Instanz für Ord
instance Ord Zahl where
  compare N N = EQ
  compare (P _) N = GT
  compare N (P _) = LT
  compare (P x) (P y) = compare x y
  compare (M x) (M y) = compare y x
  compare (M _) N = LT
  compare N (M _) = GT
  compare (P _) (M _) = GT
  compare (M _) (P _) = LT

-- Instanz für Enum
instance Enum Zahl where
  toEnum n
    | n == 0 = N
    | n > 0 = P (toEnum (n - 1))
    | otherwise = M (toEnum (n + 1))

  fromEnum N = 0
  fromEnum (P x) = 1 + fromEnum x
  fromEnum (M x) = fromEnum x - 1

-- Instanz für Num
instance Num Zahl where
  N + x = x
  x + N = x
  P x + P y = P (x + y)
  M x + M y = M (x + y)
  P x + M y = if x >= y then P (x - y) else M (y - x)
  M x + P y = if x >= y then M (x - y) else P (y - x)

  N * _ = N
  _ * N = N
  P x * P y = P (x * y)
  M x * M y = P (x * y)
  P x * M y = M (x * y)
  M x * P y = M (x * y)

  abs N = N
  abs (P x) = P x
  abs (M x) = P x

  signum N = N
  signum (P _) = P N
  signum (M _) = M N

  negate N = N
  negate (P x) = M x
  negate (M x) = P x

  fromInteger n
    | n == 0 = N
    | n > 0 = P (fromInteger (n - 1))
    | otherwise = M (fromInteger (n + 1))

-- Instanz für Show
instance Show Zahl where
  show N = "0"
  show (P x) = show $ showHelper 1 x
  show (M x) = show (-showHelper 1 x)

-- Hilfsfunktion für Show
showHelper :: Int -> Zahl -> Int
showHelper acc N = acc
showHelper acc (P x) = showHelper (acc + 1) x
showHelper acc (M x) = showHelper (acc - 1) x

-- Instanz für Read
instance Read Zahl where
  readsPrec _ s =
    [(fromInteger n, rest) | (n, rest) <- reads s]

------------------------------------------------------------------------------------
-- A2
------------------------------------------------------------------------------------
type Von = Int

type Bis = Int

type Menge = (Von, Bis) -- fuer ‘{ z | Von <= z <= Bis }’

type Paar = (Zahl, Zahl)

data Liste
  = L -- fuer ‘Leer’
  | V Liste Paar -- fuer ‘Verknuepfe’

type Relation = Liste

-- Überprüft, ob eine Relation auf einer Menge eine Relation ist
ist_Relation_auf_M :: Menge -> Relation -> Bool
ist_Relation_auf_M m r = istMenge r && allePaareInMenge m r

-- Überprüft, ob eine Relation auf einer Menge eine Reflexionsrelation ist
ist_reflexiv_auf_M :: Menge -> Relation -> Bool
ist_reflexiv_auf_M m r = istMenge r && istReflexiv m r

-- Überprüft, ob eine Relation auf einer Menge eine Transitionsrelation ist
ist_transitiv_auf_M :: Menge -> Relation -> Bool
ist_transitiv_auf_M m r = istMenge r && istTransitiv m r

-- Überprüft, ob eine Relation auf einer Menge eine Symmetriereaktion ist
ist_symmetrisch_auf_M :: Menge -> Relation -> Bool
ist_symmetrisch_auf_M m r = istMenge r && istSymmetrisch m r

-- Überprüft, ob eine Relation auf einer Menge eine Äquivalenzrelation ist
ist_Aequivalenzrelation_auf_M :: Menge -> Relation -> Bool
ist_Aequivalenzrelation_auf_M m r = istMenge r && istAequivalenzrelation m r

------------------------------------------------------------------------------------
-- A3
------------------------------------------------------------------------------------
type Personalausweisnummer = String

type Name = String

type Geschlecht = String

type Alter = Int

type Anschrift = String

type Familienstand = String

type Fuehrerscheinklasse = String

type Gemeinde = String

data Person = Person
  { personenName :: Name,
    geschlecht :: Geschlecht,
    alter :: Alter,
    ausweisnummer :: Personalausweisnummer,
    anschriften :: [Anschrift],
    familienstand :: Familienstand,
    fuehrerscheinklassen :: [Fuehrerscheinklasse]
  }
  deriving (Show, Eq)

type Personenregister = [Person]

------------------------------------------------------------------------------------
-- Hilfsfunktionen
------------------------------------------------------------------------------------
-- Überprüft, ob eine Zahl in einer Menge liegt
inMenge :: Zahl -> Menge -> Bool
inMenge zahl (v, b) = z >= v && z <= b
  where
    z = fromEnum zahl

-- Überprüft, ob ein Paar in einer Menge liegt
paarInMenge :: Paar -> Menge -> Bool
paarInMenge (x, y) m = inMenge x m && inMenge y m

-- Überprüft, ob eine Liste eine Menge repräsentiert (keine Duplikate)
istMenge :: Liste -> Bool
istMenge L = True
istMenge (V rest p) = not (paarInListe p rest) && istMenge rest

-- Überprüft, ob ein Paar in einer Liste vorkommt
paarInListe :: Paar -> Liste -> Bool
paarInListe _ L = False
paarInListe p (V rest q) = p == q || paarInListe p rest

-- Überprüft, ob eine Relation auf einer Menge eine Reflexionsrelation ist
istReflexiv :: Menge -> Relation -> Bool
istReflexiv m L = istMenge L
istReflexiv m (V rest (x, y)) = (x == y && paarInMenge (x, y) m) && istReflexiv m rest

-- Überprüft, ob eine Relation auf einer Menge eine Transitionsrelation ist
istTransitiv :: Menge -> Relation -> Bool
istTransitiv m L = istMenge L
istTransitiv m r@(V rest (x, y)) =
  alleTransitivePaare m r && istTransitiv m rest

-- Überprüft, ob alle Paare in einer Liste transitive Paare sind
alleTransitivePaare :: Menge -> Relation -> Bool
alleTransitivePaare m L = True
alleTransitivePaare m (V rest (x, y)) =
  alleTransitivePaareInListe m (V rest (x, y)) && alleTransitivePaare m rest

-- Überprüft, ob alle Paare in einer Liste transitive Paare sind
alleTransitivePaareInListe :: Menge -> Liste -> Bool
alleTransitivePaareInListe m L = True
alleTransitivePaareInListe m (V rest (x, y)) =
  istTransitivesPaar m (x, y) && alleTransitivePaareInListe m rest

-- Überprüft, ob ein Paar ein transitives Paar ist
istTransitivesPaar :: Menge -> Paar -> Bool
istTransitivesPaar m (x, y) = alleZwischenwerteInMenge m x y && alleZwischenwerteInMenge m y x

-- Überprüft, ob alle Werte zwischen zwei Zahlen in einer Menge liegen
alleZwischenwerteInMenge :: Menge -> Zahl -> Zahl -> Bool
alleZwischenwerteInMenge m x y = all (`inMenge` m) (zwischenwerte x y)

-- Erzeugt die Liste aller Werte zwischen zwei Zahlen
zwischenwerte :: Zahl -> Zahl -> [Zahl]
zwischenwerte x y
  | x == y = [x]
  | otherwise = x : zwischenwerte (nachfolger x) y

-- Gibt den Nachfolger einer Zahl zurück
nachfolger :: Zahl -> Zahl
nachfolger N = P N
nachfolger (P n) = P (P n)
nachfolger (M n) = M (P n)

-- Überprüft, ob eine Relation auf einer Menge eine Symmetriereaktion ist
istSymmetrisch :: Menge -> Relation -> Bool
istSymmetrisch m L = istMenge L
istSymmetrisch m (V rest (x, y)) =
  (x == y || (paarInMenge (x, y) m && paarInMenge (y, x) m)) && istSymmetrisch m rest

-- Überprüft, ob eine Relation auf einer Menge eine Äquivalenzrelation ist
istAequivalenzrelation :: Menge -> Relation -> Bool
istAequivalenzrelation m r =
  istReflexiv m r && istTransitiv m r && istSymmetrisch m r

-- Überprüft, ob alle Paare in einer Liste in einer Menge liegen
allePaareInMenge :: Menge -> Relation -> Bool
allePaareInMenge m L = True
allePaareInMenge m (V rest p) = paarInMenge p m && allePaareInMenge m rest
