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

-- Definition des Datentyps 'Zahl'
data Zahl
  = N -- N fuer ‘Null’
  | P Zahl -- P fuer ‘plus eins’
  | M Zahl -- M fuer ‘minus eins’
  deriving (Show)

-- Instanz fuer den Typklassen 'Enum' fuer den Datentyp 'Zahl'
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

-- Instanz fuer den Typklassen 'Eq' fuer den Datentyp 'Zahl'
instance Eq Zahl where
  (==) :: Zahl -> Zahl -> Bool
  n == m = fromEnum n == fromEnum m

-- Definition des Datentyps 'Variable'
data Variable = A | B | C | R | S | T | X | Y | Z deriving (Eq, Show)

-- Definition des Datentyps 'ArithAusdruck'
data ArithAusdruck
  = K Zahl -- K fuer Konstante
  | V Variable -- V fuer Variable
  | Ps ArithAusdruck ArithAusdruck -- Ps fuer plus
  | Ml ArithAusdruck ArithAusdruck -- Ml fuer mal
  | Ms ArithAusdruck ArithAusdruck -- Ms fuer minus
  | Abs ArithAusdruck -- Abs fuer Absolutbetrag
  deriving (Show)

-- Definition des Datentyps 'Anweisung'
data Anweisung
  = Zw
      { ls :: Variable,
        rs :: ArithAusdruck
      }
  | Skip
  | If Bedingung Block Block
  | While Bedingung Block

-- Definition des Datentyps 'LogAusdruck'
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
-- Funktion zur Ausfuehrung eines Programms
int :: Programm -> Zustand -> [Zustand]
int [] z = [z] -- Das leere Programm fuehrt zu keinem Zustandswechsel
int (anweisung : rest) z = case anweisung of
  Zw {ls = var, rs = ausdruck} ->
    let neuerWert = aw ausdruck z
        neuerZustand = updateZustand var (fromIntegral neuerWert) z
     in z : int rest neuerZustand
  Skip -> z : int rest z -- Skip fuehrt zu keinem Zustandswechsel
  If bedingung dannBlock sonstBlock ->
    if ala bedingung z
      then int (dannBlock ++ rest) z
      else int (sonstBlock ++ rest) z
  While bedingung schleifenBlock ->
    if ala bedingung z
      then int (schleifenBlock ++ [While bedingung schleifenBlock] ++ rest) z
      else z : int rest z

-- A.1.2
-- Funktion zur Inspektion eines Zustands an einem bestimmten Index
inspiziere :: Programm -> Anfangszustand -> Index -> Maybe Zustand
inspiziere [] _ _ = Nothing -- Wenn das Programm keine Anweisungen mehr hat, gibt es keinen Zustand
inspiziere _ z 0 = Just z -- Index 0 entspricht dem Anfangszustand
inspiziere prog z index
  | index >= 0 && index < length zustaende = Just $ zustaende !! index
  | otherwise = Nothing
  where
    zustaende = int prog z

-- A.1.3
-- Funktion zur Inspektion eines Zustandsintervalls
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
-- Funktion zur Inspektion eines Zustands an einem bestimmten Index mit ausgewaehlten Variablen
inspiziere2 :: Programm -> Anfangszustand -> [Variable] -> Index -> Maybe String
inspiziere2 prog z intVars index = case maybeZustand of
  Just zust -> Just $ showZustandVariables intVars zust
  Nothing -> Nothing
  where
    maybeZustand = inspiziere prog z index

-- Funktion zur Inspektion eines Zustandsintervalls mit ausgewaehlten Variablen
inspiziere2' :: Programm -> Anfangszustand -> [Variable] -> Von -> Bis -> Maybe [String]
inspiziere2' prog z intVars von bis = case maybeZustaende of
  Just zust -> Just $ map (showZustandVariables intVars) zust
  Nothing -> Nothing
  where
    maybeZustaende = inspiziere' prog z von bis

-- A.1.5
-- Funktion zur Inspektion eines Zustands an einem bestimmten Index mit ausgewaehlten Variablen und Startzustand
inspiziere3 :: Programm -> Anfangszustand -> [Variable] -> Index -> Maybe [String]
inspiziere3 prog z intVars index = case maybeZustand of
  Just zustand -> Just (showZustandVariables intVars z : [zustand])
  Nothing -> Nothing
  where
    maybeZustand = inspiziere2 prog z intVars index

-- Funktion zur Inspektion eines Zustandsintervalls mit ausgewaehlten Variablen und Startzustand
inspiziere3' :: Programm -> Anfangszustand -> [Variable] -> Von -> Bis -> Maybe [String]
inspiziere3' prog z intVars von bis = case maybeZustande of
  Just zustand -> Just (showZustandVariables intVars z : zustand)
  Nothing -> Nothing
  where
    maybeZustande = inspiziere2' prog z intVars von bis

-- A.2
-- Definition von Datentypen fuer die Loesung von Pyramidenraetseln

type Pyramid = [PyramidRow]

type Permutations = [Permutation]

type Permutation = [Int]

type PyramidRow = [Int]

-- Funktion zur Loesung eines Pyramidenraetsels
solvePyramid :: Pyramid -> Maybe Pyramid
solvePyramid [] = Nothing
solvePyramid pyramid
  | ist_pyramide_wohlgeformt pyramid (length $ head pyramid) = solvePyramidRows pyramidWithoutFirstRow pyramidWithoutFirstRow permutations [head permutations]
  | otherwise = error "Pyramide ist nicht wohlgeformt."
  where
    permutations = getPermutations (head pyramid)
    pyramidWithoutFirstRow = tail pyramid

-- Funktion zur Loesung der Reihen einer Pyramide
solvePyramidRows :: Pyramid -> Pyramid -> Permutations -> Pyramid -> Maybe Pyramid
solvePyramidRows oldPyr [] _ pyramid = Just pyramid
solvePyramidRows oldPyr (x : xs) (y : ys) newPyramid = case t of
  Just pyramid -> solvePyramidRows oldPyr xs (y : ys) (newPyramid ++ [pyramid])
  Nothing -> if null ys then Nothing else solvePyramidRows oldPyr oldPyr ys [head ys]
  where
    t = solvePyramidRow x (newPyramid !! (length newPyramid - 1)) 0 []

-- Funktion zur Loesung einer Pyramidenreihe
solvePyramidRow :: PyramidRow -> Permutation -> Index -> PyramidRow -> Maybe PyramidRow
solvePyramidRow [] _ _ row = Just row
solvePyramidRow (0 : xs) perm n newRow = solvePyramidRow xs perm (n + 1) (newRow ++ [calculatePyramidValue perm n])
solvePyramidRow (x : xs) perm n newRow =
  if x == newValue
    then solvePyramidRow xs perm (n + 1) (newRow ++ [newValue])
    else Nothing
  where
    newValue = calculatePyramidValue perm n

-- Funktion zur Berechnung des Werts einer Pyramidenposition
calculatePyramidValue :: Permutation -> Index -> Int
calculatePyramidValue perm n = (perm !! n) + (perm !! (n + 1))

-- Funktion zur Generierung aller Permutationen fuer die breiteste Reihe (Startreihe)
getPermutations :: PyramidRow -> Permutations
getPermutations lst = filterPermutations lst 0 perm
  where
    n = length lst
    perm = permutations [1 .. n]

-- Funktion zur Filterung von Permutationen basierend auf vorgegebenen Werten
filterPermutations :: PyramidRow -> Index -> Permutations -> Permutations
filterPermutations [] _ perm = perm
filterPermutations (0 : xs) n perm = filterPermutations xs (n + 1) perm
filterPermutations (x : xs) n perm = filterPermutations xs (n + 1) (filterByIndex x n perm)

-- Funktion zur Filterung von Permutationen basierend auf einem Index und Wert
filterByIndex :: Int -> Index -> Permutations -> Permutations
filterByIndex number index = filter (\lst -> length lst > index && lst !! index == number)

-- Funktion zur Ausgabe einer Pyramide in der Konsole
printPyramid :: Pyramid -> IO ()
printPyramid pyramid = mapM_ putStrLn (reverse $ map formatLine pyramid)
  where
    formatLine line = unwords (map (pad . show) line)
    maxDigitLength = maximum (map (length . show) (concat pyramid))
    pad s = replicate (maxDigitLength - length s) ' ' ++ s

ist_pyramide_wohlgeformt :: Pyramid -> Int -> Bool
ist_pyramide_wohlgeformt [] _ = True
ist_pyramide_wohlgeformt (x : xs) n
  | length x == n = ist_pyramide_wohlgeformt xs (n - 1)
  | otherwise = False

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

-- Fuehre das Programm aus und erhalte die Liste der Zustandsaenderungen
ergebnisZustaende :: [Zustand]
ergebnisZustaende = int meinProgramm meinAnfangszustand

-- Funktionen zum Ausfuehren und Testen des Programms
runA11 :: IO ()
runA11 = putStrLn $ showZustand' ergebnisZustaende

-- Zeigt die Zustandsaenderungen an
runA12 :: Index -> IO ()
runA12 x = case inspiziere meinProgramm meinAnfangszustand x of
  Just zustand -> putStrLn $ showZustand zustand
  Nothing -> putStrLn "Der angegebene Index existiert nicht."

-- Zeigt die Zustandsaenderungen im angegebenen Bereich an
runA13 :: Von -> Bis -> IO ()
runA13 von bis = case inspiziere' meinProgramm meinAnfangszustand von bis of
  Just zustand -> putStrLn $ showZustand' zustand
  Nothing -> putStrLn "Der angegebene Index existiert nicht."

-- Zeigt den Zustand zu einem bestimmten Index mit ausgewaehlten Variablen an
runA14_1 :: Index -> [Variable] -> IO ()
runA14_1 index vars = case inspiziere2 meinProgramm meinAnfangszustand vars index of
  Just zustand -> putStrLn zustand
  Nothing -> putStrLn "Der angegebene Index existiert nicht."

-- Zeigt die Zustandsaenderungen im angegebenen Bereich mit ausgewaehlten Variablen an
runA14_2 :: Von -> Bis -> [Variable] -> IO ()
runA14_2 von bis vars = case inspiziere2' meinProgramm meinAnfangszustand vars von bis of
  Just zustand -> putStrLn $ unlines zustand
  Nothing -> putStrLn "Der angegebene Index existiert nicht."

-- Zeigt den Zustand zu einem bestimmten Index mit ausgewaehlten Variablen und Startzustand an
runA15_1 :: Index -> [Variable] -> IO ()
runA15_1 index vars = case inspiziere3 meinProgramm meinAnfangszustand vars index of
  Just zustand -> putStrLn $ unlines zustand
  Nothing -> putStrLn "Der angegebene Index existiert nicht."

-- Zeigt die Zustandsaenderungen im angegebenen Bereich mit ausgewaehlten Variablen und vorherigem Zustand an
runA15_2 :: Von -> Bis -> [Variable] -> IO ()
runA15_2 von bis vars = case inspiziere3' meinProgramm meinAnfangszustand vars von bis of
  Just zustand -> putStrLn $ unlines zustand
  Nothing -> putStrLn "Der angegebene Index existiert nicht."

-- Tests A.1.2
-- Should print Zustand at index 2
testA12_1 :: IO ()
testA12_1 = runA12 2

-- Should fail to find index.
testA12_2 :: IO ()
testA12_2 = runA12 (-2)

-- Should fail to find index.
testA12_3 :: IO ()
testA12_3 = runA12 100

-- Should print last Zustand
testA12_4 :: IO ()
testA12_4 = runA12 8

-- Should print the first Zustand
testA12_5 :: IO ()
testA12_5 = runA12 0

testA12 :: IO ()
testA12 = do
  testA12_1
  testA12_2
  testA12_3
  testA12_4
  testA12_5

-- Tests A.1.3
-- Should print out all Zustaende
testA13_1 :: IO ()
testA13_1 = runA13 0 8

-- Should print out a single Zustand
testA13_2 :: IO ()
testA13_2 = runA13 1 1

-- Should fail to find index.
testA13_3 :: IO ()
testA13_3 = runA13 4 1

-- Should fail to find index.
testA13_4 :: IO ()
testA13_4 = runA13 (-1) 2

-- Should fail to find index.
testA13_5 :: IO ()
testA13_5 = runA13 1 (-2)

testA13 :: IO ()
testA13 = do
  testA13_1
  testA13_2
  testA13_3
  testA13_4
  testA13_5

-- Test A.1.4
-- Should print Zustand at index 2 with variables A, B, C
testA14_1 :: IO ()
testA14_1 = runA14_1 2 [A, B, C]

-- Should fail to find index.
testA14_2 :: IO ()
testA14_2 = runA14_1 (-2) [A, B, C]

-- Should fail to find index.
testA14_3 :: IO ()
testA14_3 = runA14_1 100 [A, B, C]

-- Should print last Zustand
testA14_4 :: IO ()
testA14_4 = runA14_1 8 [A, B, C]

-- Should print the first Zustand
testA14_5 :: IO ()
testA14_5 = runA14_1 0 [A, B, C]

testA14_6 :: IO ()
testA14_6 = runA14_2 0 8 [A, B, C]

-- Should print out a single Zustand
testA14_7 :: IO ()
testA14_7 = runA14_2 1 1 [A, B, C]

-- Should fail to find index.
testA14_8 :: IO ()
testA14_8 = runA14_2 4 1 [A, B, C]

-- Should fail to find index.
testA14_9 :: IO ()
testA14_9 = runA14_2 (-1) 2 [A, B, C]

testA14 :: IO ()
testA14 = do
  testA14_1
  testA14_2
  testA14_3
  testA14_4
  testA14_5
  testA14_6
  testA14_7
  testA14_8
  testA14_9

-- Test A.1.5
testA15_1 :: IO ()
testA15_1 = runA15_1 2 [A, B]

testA15_2 :: IO ()
testA15_2 = runA15_2 2 4 [A, B]

testA15 :: IO ()
testA15 = do
  testA15_1
  testA15_2

-- Beispiel A2

-- Beispiel-Pyramide fuer den erfolgreichen Test
testPyramid :: Pyramid
testPyramid =
  [ [2, 0, 0, 0, 4, 0],
    [0, 0, 7, 0, 0],
    [0, 13, 0, 0],
    [0, 0, 0],
    [0, 64],
    [0]
  ]

-- Beispiel-Pyramide fuer den erfolglosen Test
testPyramid2 :: Pyramid
testPyramid2 =
  [ [2, 0, 0, 0, 4, 0],
    [0, 0, 7, 0, 0],
    [0, 13, 0, 0],
    [0, 0, 0],
    [0, 64],
    [187]
  ]

-- Beispiel-Pyramide fuer den erfolgreichen Test trotz mehreren Loesungen
testPyramid3 :: Pyramid
testPyramid3 =
  [ [2, 0, 4, 0],
    [0, 0, 0],
    [0, 0],
    [0]
  ]

-- Beispiel-Pyramide ohne vorgegebenen Werten
testPyramid4 :: Pyramid
testPyramid4 =
  [ [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0],
    [0, 0],
    [0]
  ]

-- Beispiel-Pyramide ohne vorgegebenen Werten und n=10
testPyramid5 :: Pyramid
testPyramid5 =
  [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0],
    [0, 0],
    [0]
  ]

-- Beispiel-Pyramide mit vorgegebenen Werten und n=10
testPyramid6 :: Pyramid
testPyramid6 =
  [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 21, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 74, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0],
    [0, 0],
    [0]
  ]

-- Beispiel-Pyramide ohne vorgegebenen Werten und n=20
testPyramid7 :: Pyramid
testPyramid7 =
  [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0],
    [0, 0],
    [0]
  ]

-- Beispiel-Pyramide mit vorgegebenen Werten und n=20
testPyramid8 :: Pyramid
testPyramid8 =
  [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0],
    [0, 0],
    [0]
  ]

-- Testet und gibt die erfolgreich geloeste Pyramide fuer 'testPyramid' aus
testA2 :: IO ()
testA2 = case solvedPyramid of
  Just pyramid -> printPyramid pyramid
  Nothing -> putStrLn "Keine gueltige Loesung gefunden."
  where
    solvedPyramid = solvePyramid testPyramid

-- Testet und gibt eine Meldung aus, dass keine gueltige Loesung fuer 'testPyramid2' gefunden wurde
testA2_Fail :: IO ()
testA2_Fail = case solvedPyramid of
  Just pyramid -> printPyramid pyramid
  Nothing -> putStrLn "Keine gueltige Loesung gefunden."
  where
    solvedPyramid = solvePyramid testPyramid2

-- Testet und eine von mehreren moeglichen Loesungen aus
testA2_multiple_solutions :: IO ()
testA2_multiple_solutions = case solvedPyramid of
  Just pyramid -> printPyramid pyramid
  Nothing -> putStrLn "Keine gueltige Loesung gefunden."
  where
    solvedPyramid = solvePyramid testPyramid3

-- Testet und gibt erfolgreich geloeste Pyramide aus, auch ohne vergegebenen werten (6! erste Reihen)
testA2_no_given_values :: IO ()
testA2_no_given_values = case solvedPyramid of
  Just pyramid -> printPyramid pyramid
  Nothing -> putStrLn "Keine gueltige Loesung gefunden."
  where
    solvedPyramid = solvePyramid testPyramid4

-- Testet und gibt erfolgreich geloeste Pyramide aus, auch ohne vergegebenen werten (10! erste Reihen)
testA2_n_10 :: IO ()
testA2_n_10 = case solvedPyramid of
  Just pyramid -> printPyramid pyramid
  Nothing -> putStrLn "Keine gueltige Loesung gefunden."
  where
    solvedPyramid = solvePyramid testPyramid5

-- Testet und gibt eine Meldung aus, dass keine gueltige Loesung fuer 'testPyramid6' gefunden wurde, dauert merkbar laenger (10! erste Reihen)
testA2_n_10_with_values :: IO ()
testA2_n_10_with_values = case solvedPyramid of
  Just pyramid -> printPyramid pyramid
  Nothing -> putStrLn "Keine gueltige Loesung gefunden."
  where
    solvedPyramid = solvePyramid testPyramid6

-- Testet und gibt erfolgreich geloeste Pyramide aus, auch ohne vergegebenen werten, keine Performance-Probleme (20! erste Reihen)
testA2_n_20 :: IO ()
testA2_n_20 = case solvedPyramid of
  Just pyramid -> printPyramid pyramid
  Nothing -> putStrLn "Keine gueltige Loesung gefunden."
  where
    solvedPyramid = solvePyramid testPyramid7

-- Testet und gibt eine Meldung aus, dass keine gueltige Loesung fuer 'testPyramid8' gefunden wurde, dauert zu lange für termination (20! erste Reihen)
testA2_n_20_with_values :: IO ()
testA2_n_20_with_values = case solvedPyramid of
  Just pyramid -> printPyramid pyramid
  Nothing -> putStrLn "Keine gueltige Loesung gefunden."
  where
    solvedPyramid = solvePyramid testPyramid8

-- Hilfsfunktionen
-- Hilfsfunktion zur Aktualisierung des Zustands
updateZustand :: Variable -> Integer -> Zustand -> Zustand
updateZustand var wert z v = if v == var then toEnum $ fromIntegral wert else z v

-- Funktion zum Anzeigen des Zustands als String
showZustand :: Zustand -> String
showZustand z = "{ " ++ showVariables ++ " }"
  where
    variables = [A, B, C, R, S, T, X, Y, Z]
    showVariables = intercalate ", " [show v ++ " -> " ++ show (z v) | v <- variables]

-- Funktion zum Anzeigen einer Liste von Zustaenden als String
showZustand' :: [Zustand] -> String
showZustand' z = unlines $ map showZustand z

-- Funktion zum Anzeigen des Zustands mit bestimmten Variablen als String
showZustandVariables :: [Variable] -> Zustand -> String
showZustandVariables vars z = "{ " ++ showVariables ++ " }"
  where
    showVariables = intercalate ", " [show v ++ " -> " ++ show (z v) | v <- vars]

-- Funktion zur Konvertierung von Zahl zu Integer
zahlToInteger :: Zahl -> Integer
zahlToInteger = toInteger . fromEnum

-- Funktion zum Auswerten arithmetischer Ausdruecke
aw :: ArithAusdruck -> Zustand -> Wert
aw (K zahl) _ = zahlToInteger zahl
aw (V var) z = zahlToInteger (z var)
aw (Ps e1 e2) z = aw e1 z + aw e2 z
aw (Ml e1 e2) z = aw e1 z * aw e2 z
aw (Ms e1 e2) z = aw e1 z - aw e2 z
aw (Abs e) z = abs (aw e z)

-- Funktion zur Auswertung logischer Ausdruecke
ala :: LogAusdruck -> Zustand -> Wahrheitswert
ala (LK w) _ = w
ala (Und l1 l2) z = ala l1 z && ala l2 z
ala (Oder l1 l2) z = ala l1 z || ala l2 z
ala (Nicht l) z = not (ala l z)
ala (Gleich a1 a2) z = aw a1 z == aw a2 z
ala (Kleiner a1 a2) z = aw a1 z < aw a2 z
