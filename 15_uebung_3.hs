{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module UE3 where

import Data.Bifunctor
import Data.List (nub)

data Strichzahl
  = S' -- S' fuer 'ein Strich'
  | S Strichzahl -- S fuer 'ein Strich mehr'

newtype Relation = R [(Strichzahl, Strichzahl)]

------------------------------------------------------------------------------------
-- A1
------------------------------------------------------------------------------------
-- Instanz fuer Eq
instance Eq Strichzahl where
  S' == S' = True
  S a == S b = a == b
  _ == _ = False

-- Instanz fuer Ord
instance Ord Strichzahl where
  S' <= _ = True
  (S a) <= S' = False
  (S a) <= (S b) = a <= b

-- Instanz fuer Enum
instance Enum Strichzahl where
  toEnum n
    | n < 1 = S'
    | otherwise = S $ toEnum (n - 1)
  fromEnum S' = 1
  fromEnum (S a) = 1 + fromEnum a

-- Instanz fuer Num
instance Num Strichzahl where
  S' + S' = S S'
  S' + S b = S b
  S a + S' = S a
  S a + S b = S (S a + b)

  S' * _ = S'
  _ * S' = S'
  S a * S b = a * (S a * b) + S a

  abs a = a

  signum S' = S'
  signum a = a

  fromInteger n
    | n < 1 = S'
    | otherwise = S $ fromInteger (n - 1)

  negate S' = S'
  negate (S a) = error "Negierung ist nicht definiert fuer natuerliche Zahlen."

-- Instanz fuer Show
instance Show Strichzahl where
  show S' = "|"
  show (S a) = "|" ++ show a

-- Instanz fuer Read
instance Read Strichzahl where
  readsPrec _ s = [(fromString s, "")]

------------------------------------------------------------------------------------
-- A2
------------------------------------------------------------------------------------
ist_gueltig :: Relation -> Bool
ist_gueltig (R pairs) = length pairs == length (nub pairs)

instance Eq Relation where
  (R r1) == (R r2)
    | not $ ist_gueltig (R r1) = error "Relation 1 ist nicht gueltig."
    | not $ ist_gueltig (R r2) = error "Relation 2 ist nicht gueltig."
    | otherwise = nub r1 == nub r2

instance Ord Relation where
  compare (R r1) (R r2)
    | not $ ist_gueltig (R r1) = error "Relation 1 ist nicht gueltig."
    | not $ ist_gueltig (R r2) = error "Relation 2 ist nicht gueltig."
    | otherwise = compare (nub r1) (nub r2)

instance Num Relation where
  (+) = unionRelations
  (*) = composeRelations
  abs = id
  signum _ = error "Signum nicht definiert fuer Relationen."
  negate (R r)
    | not $ ist_gueltig (R r) = error "Relation ist nicht gueltig."
    | otherwise = R (map (bimap negateStrichzahl negateStrichzahl) r)
  fromInteger n
    | n < 0 = error "Negative integer sind nicht definiert fuer Relationen."
    | otherwise = R []

instance Show Relation where
  show (R r)
    | not $ ist_gueltig (R r) = error "Relation ist nicht gueltig."
    | otherwise = "{" ++ showPairs r ++ "}"
    where
      showPairs [] = ""
      showPairs [(a, b)] = "(" ++ show a ++ "," ++ show b ++ ")"
      showPairs ((a, b) : rest) = "(" ++ show a ++ "," ++ show b ++ ")" ++ "," ++ showPairs rest

{-
instance Read Relation where
  readsPrec _ s =
    case parseRelation s of
      Just rel -> [(rel, "")]
      Nothing -> error "Invalid Relation format"
-}

------------------------------------------------------------------------------------
-- A3
------------------------------------------------------------------------------------

type Umfang = Float

type Flaeche = Float

type Volumen = Float

class Figur a where
  umfang :: a -> Umfang
  flaeche :: a -> Flaeche
  volumen :: a -> Volumen

-- Definition der Datentypen für zweidimensionale Figuren
data Quadrat = Quadrat {seitenlaenge :: Float}

data Rechteck = Rechteck {laenge :: Float, breite :: Float}

data Kreis = Kreis {radius :: Float}

-- Definition der Datentypen für dreidimensionale Figuren
data Wuerfel = Wuerfel {kantenlaenge :: Float}

data Quader = Quader {langeSeite :: Float, kurzeSeite :: Float, hoehe :: Float}

data Kugel = Kugel {durchmesser :: Float}

-- Instanziierung der Typklasse Figur für die verschiedenen Figuren
instance Figur Quadrat where
  umfang quadrat = 4 * seitenlaenge quadrat
  flaeche quadrat = seitenlaenge quadrat ^ 2
  volumen _ = 0 -- Das Volumen eines zweidimensionalen Objekts ist als null festgelegt

instance Figur Rechteck where
  umfang rechteck = 2 * (laenge rechteck + breite rechteck)
  flaeche rechteck = laenge rechteck * breite rechteck
  volumen _ = 0

instance Figur Kreis where
  umfang kreis = 2 * pi * radius kreis
  flaeche kreis = pi * radius kreis ^ 2
  volumen _ = 0

instance Figur Wuerfel where
  umfang wuerfel = 12 * kantenlaenge wuerfel
  flaeche wuerfel = 6 * kantenlaenge wuerfel ^ 2
  volumen wuerfel = kantenlaenge wuerfel ^ 3

instance Figur Quader where
  umfang quader = 4 * (langeSeite quader + kurzeSeite quader)
  flaeche quader = 2 * (langeSeite quader * kurzeSeite quader + kurzeSeite quader * hoehe quader + langeSeite quader * hoehe quader)
  volumen quader = langeSeite quader * kurzeSeite quader * hoehe quader

instance Figur Kugel where
  umfang kugel = pi * durchmesser kugel
  flaeche kugel = 4 * pi * (durchmesser kugel / 2) ^ 2
  volumen kugel = (4 / 3) * pi * (durchmesser kugel / 2) ^ 3

------------------------------------------------------------------------------------
-- Hilfsfunktionen
------------------------------------------------------------------------------------
fromString :: String -> Strichzahl
fromString ['|'] = S'
fromString ('|' : xs) = S $ fromString xs
fromString _ = error "Invalider Character."

-- Funktion zur Vereinigung von Relationen
unionRelations :: Relation -> Relation -> Relation
unionRelations (R r1) (R r2)
  | not $ ist_gueltig (R r1) = error "Relation 1 ist nicht gueltig."
  | not $ ist_gueltig (R r2) = error "Relation 2 ist nicht gueltig."
  | otherwise = R (nub (r1 ++ r2))

-- Funktion zur Verknüpfung von Relationen (Komposition)
composeRelations :: Relation -> Relation -> Relation
composeRelations (R r1) (R r2)
  | not $ ist_gueltig (R r1) = error "Relation 1 ist nicht gueltig."
  | not $ ist_gueltig (R r2) = error "Relation 2 ist nicht gueltig."
  | otherwise = R [(a, c) | (a, b1) <- r1, (b2, c) <- r2, b1 == b2]

negateStrichzahl :: Strichzahl -> Strichzahl
negateStrichzahl S' = S'
negateStrichzahl (S a) = S (negateStrichzahl a)

{-
-- Function to parse a Relation from a string
parseRelation :: String -> Maybe Relation
parseRelation s =
  case stripBraces s of
    Just content -> Just (R (parsePairs content))
    Nothing -> Nothing

-- Function to strip the outer braces if they exist
stripBraces :: String -> Maybe String
stripBraces rel
  | head rel == '{' && last rel == '}' = Just (filter (`notElem` "{}") rel)
  | otherwise = Nothing

-- Function to parse pairs inside the braces
parsePairs :: String -> [(Strichzahl, Strichzahl)]
parsePairs s = case break (== '}') s of
  (pairStr, rest) ->
    if null pairStr
      then []
      else case parsePair pairStr of
        Just pair -> pair : parsePairs (dropWhile (== ',') rest)
        Nothing -> error "Invalid pair format"

-- Function to parse a single pair
parsePair :: String -> Maybe (Strichzahl, Strichzahl)
parsePair s = case break (== ',') s of
  (leftStr, ',' : rightStr) ->
    case (parseStrichzahl leftStr, parseStrichzahl rightStr) of
      (Just left, Just right) -> Just (left, right)
      _ -> Nothing
  _ -> Nothing

parsePair' :: String -> (String, String)
parsePair' input = (first, second)
  where
    (firstPart, rest) = break (== ',') input
    first = drop 1 firstPart -- drop the leading '('
    second = takeWhile (/= ')') $ tail rest

splitPairs :: String -> [(String, String)]
splitPairs input = map parsePair' pairs
  where
    pairs = words $ map (\c -> if c == ',' || c == '(' || c == ')' then ' ' else c) input

-- Function to parse a Strichzahl
parseStrichzahl :: String -> Maybe Strichzahl
parseStrichzahl ['|'] = Just S'
parseStrichzahl ('|' : xs) = case parseStrichzahl xs of
  Just num -> Just (S num)
  Nothing -> Nothing
parseStrichzahl _ = Nothing
-}
