-- allows adding type signatures to typeclass functions when creating instances
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use first" #-}
-- for IDE to ignore certain suggestion
{-# HLINT ignore "Use camelCase" #-}
-- for IDE to ignore certain suggestion
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module UE7 where

import Data.Foldable
import Data.List (find, intercalate, permutations)
import Data.Maybe (isNothing)

data Zahl
  = N -- N fuer ‘Null’
  | P Zahl -- P fuer ‘plus eins’
  | M Zahl -- M fuer ‘minus eins’
  deriving (Show)

instance Enum Zahl where
  toEnum :: Int -> Zahl
  toEnum n
    | n == 0 = N
    | n > 0 = P (toEnum (n - 1))
    | n < 0 = M (toEnum (n + 1))

  fromEnum :: Zahl -> Int
  fromEnum N = 0
  fromEnum (P n) = 1 + fromEnum n
  fromEnum (M n) = (-1) + fromEnum n

instance Eq Zahl where
  (==) :: Zahl -> Zahl -> Bool
  n == m = fromEnum n == fromEnum m

data Variable = A | B | C | R | S | T | X | Y | Z deriving (Eq, Show)

data ArithAusdruck
  = K Zahl -- K fuer Konstante
  | V Variable -- V fuer Variable
  | Ps ArithAusdruck ArithAusdruck -- Ps fuer plus
  | Ml ArithAusdruck ArithAusdruck -- Ml fuer mal
  | Ms ArithAusdruck ArithAusdruck -- Ms fuer minus
  | Abs ArithAusdruck -- Abs fuer Absolutbetrag
  deriving (Show)

data Anweisung
  = Zw
      { ls :: Variable,
        rs :: ArithAusdruck
      }
  | Skip
  | If Bedingung Block Block
  | While Bedingung Block

data LogAusdruck
  = LK Wahrheitswert -- LK fuer logische Konstante
  | Und LogAusdruck LogAusdruck -- Logische Konjunktion
  | Oder LogAusdruck LogAusdruck -- Logische Disjunktion
  | Nicht LogAusdruck -- Logische Negation
  | Gleich ArithAusdruck ArithAusdruck -- Wertegleichheit zweier arithm. Ausdruecke
  | Kleiner ArithAusdruck ArithAusdruck -- Wert des ersten Ausdruck kleiner Wert des zweiten

type Bedingung = LogAusdruck

type Block = [Anweisung]

type Programm = Block

type Zustand = Variable -> Zahl

type Wert = Integer

type Wahrheitswert = Bool

type Nat0 = Int

type Anfangszustand = Zustand

type Index = Nat0

type Von = Index

type Bis = Index

-- A.1.1
int :: Programm -> Zustand -> [Zustand]
int [] z = [z] -- Das leere Programm führt zu keinem Zustandswechsel
int (anweisung : rest) z = case anweisung of
  Zw {ls = var, rs = ausdruck} ->
    let neuerWert = aw ausdruck z
        neuerZustand = updateZustand var (fromIntegral neuerWert) z
     in z : int rest neuerZustand
  Skip -> z : int rest z -- Skip führt zu keinem Zustandswechsel
  If bedingung dannBlock sonstBlock ->
    if ala bedingung z
      then int (dannBlock ++ rest) z
      else int (sonstBlock ++ rest) z
  While bedingung schleifenBlock ->
    if ala bedingung z
      then int (schleifenBlock ++ [While bedingung schleifenBlock] ++ rest) z
      else z : int rest z

-- A.1.2
inspiziere :: Programm -> Anfangszustand -> Index -> Maybe Zustand
inspiziere [] _ _ = Nothing -- Wenn das Programm keine Anweisungen mehr hat, gibt es keinen Zustand
inspiziere _ z 0 = Just z -- Index 0 entspricht dem Anfangszustand
inspiziere prog z index
  | index >= 0 && index < length zustaende = Just $ zustaende !! index
  | otherwise = Nothing
  where
    zustaende = int prog z

-- A.1.3
inspiziere' :: Programm -> Anfangszustand -> Von -> Bis -> Maybe [Zustand]
inspiziere' [] _ _ _ = Nothing
inspiziere' prog z von bis
  | von > bis = Nothing
  | von == bis && von >= 0 && von < length zustaende = Just [zustaende !! von]
  | von >= 0 && bis < length zustaende = Just (take (bis - von + 1) $ drop von zustaende)
  | otherwise = Nothing
  where
    zustaende = int prog z

-- A.1.4
inspiziere2 :: Programm -> Anfangszustand -> [Variable] -> Index -> Maybe String
inspiziere2 prog z intVars index = case maybeZustand of
  Just zust -> Just $ showZustandVariables intVars zust
  Nothing -> Nothing
  where
    maybeZustand = inspiziere prog z index

inspiziere2' :: Programm -> Anfangszustand -> [Variable] -> Von -> Bis -> Maybe [String]
inspiziere2' prog z intVars von bis = case maybeZustaende of
  Just zust -> Just $ map (showZustandVariables intVars) zust
  Nothing -> Nothing
  where
    maybeZustaende = inspiziere' prog z von bis

-- A.1.5
inspiziere3 :: Programm -> Anfangszustand -> [Variable] -> Index -> Maybe [String]
inspiziere3 prog z intVars index = case maybeZustand of
  Just zustand -> Just (showZustandVariables intVars z : [zustand])
  Nothing -> Nothing
  where
    maybeZustand = inspiziere2 prog z intVars index

inspiziere3' :: Programm -> Anfangszustand -> [Variable] -> Von -> Bis -> Maybe [String]
inspiziere3' prog z intVars von bis = case maybeZustande of
  Just zustand -> Just (showZustandVariables intVars z : zustand)
  Nothing -> Nothing
  where
    maybeZustande = inspiziere2' prog z intVars von bis

-- A.2
type Pyramid = [PyramidRow]

type Permutations = [Permutation]

type Permutation = [Int]

type PyramidRow = [Int]

solvePyramid :: Pyramid -> Maybe Pyramid
solvePyramid [] = Nothing
solvePyramid pyramid = solvePyramidRows pyramidWithoutFirstRow pyramidWithoutFirstRow permutations [head permutations]
  where
    permutations = getPermutations (head pyramid)
    pyramidWithoutFirstRow = tail pyramid

solvePyramidRows :: Pyramid -> Pyramid -> Permutations -> Pyramid -> Maybe Pyramid
solvePyramidRows oldPyr [] _ pyramid = Just pyramid
solvePyramidRows oldPyr (x : xs) (y : ys) newPyramid = case t of
  Just pyramid -> solvePyramidRows oldPyr xs (y : ys) (newPyramid ++ [pyramid])
  Nothing -> if null ys then Nothing else solvePyramidRows oldPyr oldPyr ys [head ys]
  where
    t = solvePyramidRow x (newPyramid !! (length newPyramid - 1)) 0 []

solvePyramidRow :: PyramidRow -> Permutation -> Index -> PyramidRow -> Maybe PyramidRow
solvePyramidRow [] _ _ row = Just row
solvePyramidRow (0 : xs) perm n newRow = solvePyramidRow xs perm (n + 1) (newRow ++ [calculatePyramidValue perm n])
solvePyramidRow (x : xs) perm n newRow =
  if x == newValue
    then solvePyramidRow xs perm (n + 1) (newRow ++ [newValue])
    else Nothing
  where
    newValue = calculatePyramidValue perm n

calculatePyramidValue :: Permutation -> Index -> Int
calculatePyramidValue perm n = (perm !! n) + (perm !! (n + 1))

getPermutations :: PyramidRow -> Permutations
getPermutations lst = filterPermutations lst 0 perm
  where
    n = length lst
    perm = permutations [1 .. n]

filterPermutations :: PyramidRow -> Index -> Permutations -> Permutations
filterPermutations [] _ perm = perm
filterPermutations (0 : xs) n perm = filterPermutations xs (n + 1) perm
filterPermutations (x : xs) n perm = filterPermutations xs (n + 1) (filterByIndex x n perm)

filterByIndex :: Int -> Index -> Permutations -> Permutations
filterByIndex number index = filter (\lst -> length lst > index && lst !! index == number)

printPyramid :: Pyramid -> IO ()
printPyramid pyramid = mapM_ putStrLn (reverse $ map formatLine pyramid)
  where
    formatLine line = unwords (map (pad . show) line)
    maxDigitLength = maximum (map (length . show) (concat pyramid))
    pad s = replicate (maxDigitLength - length s) ' ' ++ s

-- Beispiel A1
meinProgramm :: Programm
meinProgramm =
  [ Zw {ls = A, rs = K (toEnum 3)}, -- Setze A auf 3
    Zw {ls = B, rs = K (toEnum 2)}, -- Setze B auf 2
    Zw {ls = C, rs = K (toEnum 0)}, -- Setze C auf 0
    While
      (Kleiner (V A) (K (toEnum 5))) -- Solange A < 5
      [ Zw {ls = C, rs = Ps (V C) (V A)}, -- C += A
        Zw {ls = A, rs = Ps (V A) (K (toEnum 1))} -- A += 1
      ]
  ]

-- Anfangszustand: Alle Variablen sind auf Null gesetzt
meinAnfangszustand :: Zustand
meinAnfangszustand _ = N

-- Führe das Programm aus und erhalte die Liste der Zustandsänderungen
ergebnisZustaende :: [Zustand]
ergebnisZustaende = int meinProgramm meinAnfangszustand

testA11 :: IO ()
testA11 = putStrLn $ showZustand' ergebnisZustaende

testA12 :: Index -> IO ()
testA12 x = case inspiziere meinProgramm meinAnfangszustand x of
  Just zustand -> putStrLn $ showZustand zustand
  Nothing -> putStrLn "Der angegebene Index existiert nicht."

testA13 :: Von -> Bis -> IO ()
testA13 von bis = case inspiziere' meinProgramm meinAnfangszustand von bis of
  Just zustand -> putStrLn $ showZustand' zustand
  Nothing -> putStrLn "Der angegebene Index existiert nicht."

testA14_1 :: Index -> [Variable] -> IO ()
testA14_1 index vars = case inspiziere2 meinProgramm meinAnfangszustand vars index of
  Just zustand -> putStrLn zustand
  Nothing -> putStrLn "Der angegebene Index existiert nicht."

testA14_2 :: Von -> Bis -> [Variable] -> IO ()
testA14_2 von bis vars = case inspiziere2' meinProgramm meinAnfangszustand vars von bis of
  Just zustand -> putStrLn $ unlines zustand
  Nothing -> putStrLn "Der angegebene Index existiert nicht."

testA15_1 :: Index -> [Variable] -> IO ()
testA15_1 index vars = case inspiziere3 meinProgramm meinAnfangszustand vars index of
  Just zustand -> putStrLn $ unlines zustand
  Nothing -> putStrLn "Der angegebene Index existiert nicht."

testA15_2 :: Von -> Bis -> [Variable] -> IO ()
testA15_2 von bis vars = case inspiziere3' meinProgramm meinAnfangszustand vars von bis of
  Just zustand -> putStrLn $ unlines zustand
  Nothing -> putStrLn "Der angegebene Index existiert nicht."

-- Beispiel A2

testPyramid :: Pyramid
testPyramid =
  [ [2, 0, 0, 0, 4, 0],
    [0, 0, 7, 0, 0],
    [0, 13, 0, 0],
    [0, 0, 0],
    [0, 64],
    [0]
  ]

testPyramid2 :: Pyramid
testPyramid2 =
  [ [2, 0, 0, 0, 4, 0],
    [0, 0, 7, 0, 0],
    [0, 13, 0, 0],
    [0, 0, 0],
    [0, 64],
    [187]
  ]

testA2 :: IO ()
testA2 = case solvedPyramid of
  Just pyramid -> printPyramid pyramid
  Nothing -> putStrLn "Keine gueltige Loesung gefunden."
  where
    solvedPyramid = solvePyramid testPyramid

testA2_Fail :: IO ()
testA2_Fail = case solvedPyramid of
  Just pyramid -> printPyramid pyramid
  Nothing -> putStrLn "Keine gueltige Loesung gefunden."
  where
    solvedPyramid = solvePyramid testPyramid2

-- Hilfsfunktionen
-- Hilfsfunktion zur Aktualisierung des Zustands
updateZustand :: Variable -> Integer -> Zustand -> Zustand
updateZustand var wert z v = if v == var then toEnum $ fromIntegral wert else z v

showZustand :: Zustand -> String
showZustand z = "{ " ++ showVariables ++ " }"
  where
    variables = [A, B, C, R, S, T, X, Y, Z]
    showVariables = intercalate ", " [show v ++ " -> " ++ show (z v) | v <- variables]

showZustand' :: [Zustand] -> String
showZustand' z = unlines $ map showZustand z

showZustandVariables :: [Variable] -> Zustand -> String
showZustandVariables vars z = "{ " ++ showVariables ++ " }"
  where
    showVariables = intercalate ", " [show v ++ " -> " ++ show (z v) | v <- vars]

zahlToInteger :: Zahl -> Integer
zahlToInteger = toInteger . fromEnum

aw :: ArithAusdruck -> Zustand -> Wert
aw (K zahl) _ = zahlToInteger zahl
aw (V var) z = zahlToInteger (z var)
aw (Ps e1 e2) z = aw e1 z + aw e2 z
aw (Ml e1 e2) z = aw e1 z * aw e2 z
aw (Ms e1 e2) z = aw e1 z - aw e2 z
aw (Abs e) z = abs (aw e z)

ala :: LogAusdruck -> Zustand -> Wahrheitswert
ala (LK w) _ = w
ala (Und l1 l2) z = ala l1 z && ala l2 z
ala (Oder l1 l2) z = ala l1 z || ala l2 z
ala (Nicht l) z = not (ala l z)
ala (Gleich a1 a2) z = aw a1 z == aw a2 z
ala (Kleiner a1 a2) z = aw a1 z < aw a2 z
