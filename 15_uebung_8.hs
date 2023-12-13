{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module UE8 where

import Data.Char (isDigit)
import Data.List (permutations, transpose)
import Debug.Trace (trace)

div_and_conquer ::
  (p -> Bool) -> -- Ist das Problem einfach genug, um sofort gelöst werden zu können?
  (p -> s) -> -- Löst `einfach-genug'-Probleme
  (p -> [p]) -> -- Teile: zerlegt `nicht-einfach-genug'-Probleme in kleinere
  (p -> [s] -> s) -> -- Herrsche: setzt die Lsg. der kleineren Teilprobleme zur Lsg. des größeren Ausgangsproblems zusammen
  p -> -- das zu lösende Problem
  s -- die gesuchte Lösung
div_and_conquer is_simple_enough solve divide combine = dac
  where
    dac pbi
      | is_simple_enough pbi = solve pbi
      | otherwise = combine pbi (map dac (divide pbi))

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq)

minimum' :: Ord a => [a] -> a
minimum' = div_and_conquer m_is_simple_enough m_solve m_divide m_combine

odds :: Integral a => [a] -> a
odds = div_and_conquer o_is_simple_enough o_solve o_divide o_combine

nodes :: Tree a -> Integer
nodes = div_and_conquer n_is_simple_enough n_solve n_divide n_combine

-- A.1.a
m_is_simple_enough :: Ord a => [a] -> Bool
m_is_simple_enough list = length list <= 1

m_solve :: Ord a => [a] -> a
m_solve [x] = x
m_solve _ = error "m_solve: the list should contain exactly one element"

m_divide :: Ord a => [a] -> [[a]]
m_divide [] = []
m_divide [x] = [[x]]
m_divide xs = [take half xs, drop half xs]
  where
    half = length xs `div` 2

m_combine :: Ord a => [a] -> [a] -> a
m_combine _ = minimum

-- A.1.b
o_is_simple_enough :: Integral a => [a] -> Bool
o_is_simple_enough list = length list <= 1

o_solve :: Integral a => [a] -> a
o_solve [] = 0
o_solve [x]
  | odd x = x
  | otherwise = 0

o_divide :: Integral a => [a] -> [[a]]
o_divide [] = []
o_divide [x] = [[x]]
o_divide xs = [take half xs, drop half xs]
  where
    half = length xs `div` 2

o_combine :: Integral a => [a] -> [a] -> a
o_combine _ = sum

-- A.1.c
n_is_simple_enough :: Tree a -> Bool
n_is_simple_enough Nil = True
n_is_simple_enough Node {} = False

n_solve :: Tree a -> Integer
n_solve Nil = 0
n_solve (Node l _ r) = 1 + nodes l + nodes r

n_divide :: Tree a -> [Tree a]
n_divide Nil = []
n_divide (Node l _ r) = [l, r]

n_combine :: Tree a -> [Integer] -> Integer
n_combine _ [x, y] = 1 + x + y

-- A.2
merge_sort :: Ord a => [a] -> [a]
merge_sort = div_and_conquer e_is_simple_enough e_solve e_divide e_combine

e_is_simple_enough :: Ord a => [a] -> Bool
e_is_simple_enough ls = length ls <= 1

e_solve :: Ord a => [a] -> [a]
e_solve = id

e_divide :: Ord a => [a] -> [[a]]
e_divide [] = []
e_divide [x] = [[x]]
e_divide xs = [take half xs, drop half xs]
  where
    half = length xs `div` 2

e_combine :: Ord a => [a] -> [[a]] -> [a]
e_combine _ [sorted1, sorted2] = merge sorted1 sorted2

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- A.3
generiere_fib_strom :: [Integer]
generiere_fib_strom = 0 : 1 : zipWith (+) generiere_fib_strom (tail generiere_fib_strom)

-- A.4
type Epsilon = Double

approximiere_exp :: Double -> Epsilon -> Double
approximiere_exp x epsilon = sum $ exp_selektor epsilon $ exp_generator x

exp_generator :: Double -> [Double]
exp_generator x = [x ^ n / fromIntegral (fakultaet n) | n <- [0 ..]]

exp_selektor :: Epsilon -> [Double] -> [Double]
exp_selektor epsilon exp = take n exp
  where
    n = head [n | n <- [2 ..], abs (sum (take n exp) - sum (take (n - 1) exp)) <= epsilon]

fakultaet :: Integer -> Integer
fakultaet n
  | n == 0 = 1
  | otherwise = n * fakultaet (n - 1)

-- A.5.a
type Woerterstrom = [String]

{-
generiere_woerter :: Woerterstrom
generiere_woerter = "" : go [""]
  where
    go current@(w : ws) = nextW : go (nextW : current)
      where
        nextW = incrementWord w

incrementWord :: String -> String
incrementWord [] = "a"
incrementWord (c : cs)
  | c == 'c' = 'a' : incrementWord cs
  | otherwise = succ c : cs
-}

generiere_woerter :: Woerterstrom
generiere_woerter = "" : wort_gen ['a', 'b', 'c']

-- A.5.b
filtere_prim_a :: Woerterstrom -> Woerterstrom
filtere_prim_a = filter (isPrim . anzahl_a 'a')

anzahl_a :: Char -> String -> Int
anzahl_a ch = length . filter (== ch)

isPrim :: Int -> Bool
isPrim n
  | n <= 1 = False
  | otherwise = all (\x -> n `mod` x /= 0) [2 .. intSqrt n]

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

-- A.6.a
filtere_palindrome :: Woerterstrom -> Woerterstrom
filtere_palindrome = filter istPalindrom

istPalindrom :: String -> Bool
istPalindrom word = word == reverse word

-- A.6.b
type Wort = String

transf :: Woerterstrom -> Woerterstrom
transf = map transfEinzelnesWort

transfEinzelnesWort :: Wort -> Wort
transfEinzelnesWort = map toUpper

-- Hierbei handelt es sich um eine Hilfsfunktion, um den Großbuchstaben zu erhalten
toUpper :: Char -> Char
toUpper ch
  | 'a' <= ch && ch <= 'z' = toEnum (fromEnum ch - fromEnum 'a' + fromEnum 'A')
  | otherwise = ch

-- A.7
wort_gen :: [Char] -> [[Char]]
wort_gen [] = []
wort_gen chars
  | any isDigit chars = error "Ungueltige Eingabe."
  | otherwise = concatMap (`replicateM` chars) [1 ..]
  where
    replicateM n = sequence . replicate n

-- A.8
-- Datenstruktur für einen Weihnachtsbaum
type Weihnachtsbaum = Int

type Weihnachtsbaumreihe = [Weihnachtsbaum]

type Weihnachtsbaumflaeche = [Weihnachtsbaumreihe]

data Richtung = Links | Rechts | Unten | Oben deriving (Show, Eq)

data MA2412Problem = MA2412Problem
  { weihnachtsbaumflaeche :: Weihnachtsbaumflaeche,
    vorgabenLinks :: [Int],
    vorgabenRechts :: [Int],
    vorgabenUnten :: [Int],
    vorgabenOben :: [Int]
  }
  deriving (Show)

istLoesung :: MA2412Problem -> Bool
istLoesung problem =
  and
    [ istLoesungInRichtung Links,
      istLoesungInRichtung Rechts,
      istLoesungInRichtung Unten,
      istLoesungInRichtung Oben
    ]
  where
    istLoesungInRichtung :: Richtung -> Bool
    istLoesungInRichtung richtung =
      and $
        zipWith
          (\vorgabe reihe -> vorgabe == trace ("Reihe: " ++ show reihe ++ ", Richtung: " ++ show richtung ++ ", Vorgabe: " ++ show vorgabe ++ ", Result: " ++ show (anzahlSichtbareBaeume richtung reihe)) anzahlSichtbareBaeume richtung reihe)
          (vorgaben richtung)
          (weihnachtsbaumflaeche problem)

    anzahlSichtbareBaeume :: Richtung -> Weihnachtsbaumreihe -> Int
    anzahlSichtbareBaeume Links l@(x : xs) = length $ filter (> x) l
    anzahlSichtbareBaeume Rechts l@(x : xs) = length $ reverse (filter (> x) l)
    anzahlSichtbareBaeume Unten l@(x : xs) = length $ filter (> x) l
    anzahlSichtbareBaeume Oben l@(x : xs) = length $ reverse (filter (> x) l)

    vorgaben :: Richtung -> [Int]
    vorgaben Links = vorgabenLinks problem
    vorgaben Rechts = vorgabenRechts problem
    vorgaben Unten = vorgabenUnten problem
    vorgaben Oben = vorgabenOben problem

countGreaterThanHead :: Weihnachtsbaumreihe -> Int -> Int
countGreaterThanHead [] _ = 0
countGreaterThanHead (0 : xs) lastMax = 1 + countGreaterThanHead xs lastMax
countGreaterThanHead (x : xs) lastMax
  | x == lastMax = trace ("calling countGreaterThanHead case 1 x = " ++ show x ++ ", lastMax = " ++ show lastMax) 1
  | x >= lastMax = trace ("calling countGreaterThanHead case 2 x = " ++ show x ++ ", lastMax = " ++ show lastMax) 1 + countGreaterThanHead xs x
  | otherwise = trace ("calling countGreaterThanHead case 3 x = " ++ show x ++ ", lastMax = " ++ show lastMax) 0

t :: Weihnachtsbaumreihe -> Int -> Int
t [] _ = 0
t (x : xs) max
  | x > max = 1 + t xs x
  | otherwise = 0

t1 :: Weihnachtsbaumreihe
t1 = [10, 0, 30, 0, 50]

t2 :: Weihnachtsbaumreihe
t2 = [50, 0, 10, 0, 30]

t3 :: Weihnachtsbaumreihe
t3 = [50, 50, 10, 0, 30]

-- Beispiele A.8
beispiel1 :: MA2412Problem
beispiel1 =
  MA2412Problem
    { weihnachtsbaumflaeche =
        [ [10, 0, 30, 0, 50],
          [0, 30, 0, 50, 0],
          [0, 0, 0, 10, 20],
          [40, 50, 0, 20, 0],
          [50, 0, 20, 0, 40]
        ],
      vorgabenLinks = [5, 4, 3, 2, 1],
      vorgabenRechts = [1, 2, 2, 2, 2],
      vorgabenUnten = [1, 2, 2, 2, 2],
      vorgabenOben = [5, 4, 3, 2, 1]
    }

beispiel2 :: MA2412Problem
beispiel2 =
  MA2412Problem
    { weihnachtsbaumflaeche =
        [ [10, 20, 30, 40, 50],
          [20, 30, 40, 50, 10],
          [30, 40, 50, 10, 20],
          [40, 50, 10, 20, 30],
          [50, 10, 20, 30, 40]
        ],
      vorgabenLinks = [5, 4, 3, 2, 1],
      vorgabenRechts = [1, 2, 2, 2, 2],
      vorgabenUnten = [1, 2, 2, 2, 2],
      vorgabenOben = [5, 4, 3, 2, 1]
    }

-- Tests
runAllTests :: IO ()
runAllTests = do
  runA1tests
  runA2tests
  runA3tests
  runA4tests
  runA5tests
  runA6tests
  runA7tests

-- A.1
testA1_Minimum :: Ord a => [a] -> a -> Bool
testA1_Minimum list expected = actual == expected
  where
    actual = minimum' list

testA1_Odds :: Integral a => [a] -> a -> Bool
testA1_Odds list expected = actual == expected
  where
    actual = odds list

testA1_nodes :: Tree a -> Integer -> Bool
testA1_nodes tree expected = actual == expected
  where
    actual = nodes tree

-- Beispiele
-- tree1-Baum:
--       1
--      / \
--     2   3
--    / \
--   4   5
tree1 :: Tree Int
tree1 = Node (Node (Node Nil 4 Nil) 2 (Node Nil 5 Nil)) 1 (Node Nil 3 Nil)

tree2 :: Tree String
tree2 = Node Nil "Test" Nil

{-
Input: [], Expected Output: error "Input should have at least one element."
-}
runA1tests :: IO ()
runA1tests = do
  putStrLn "A1 minimum':"
  putStrLn ("Input: [5,4,3,2,1], Expected Output: 1, Success: " ++ show (testA1_Minimum [5, 4, 3, 2, 1] 1))
  putStrLn ("Input: [1,4,3,2,1], Expected Output: 1, Success: " ++ show (testA1_Minimum [1, 4, 3, 2, 1] 1))
  putStrLn ("Input: [5], Expected Output: 5, Success: " ++ show (testA1_Minimum [5] 5))
  putStrLn "\nA1 odds:"
  putStrLn ("Input: [5,4,3,2,1], Expected Output: 9, Success: " ++ show (testA1_Odds [5, 4, 3, 2, 1] 9))
  putStrLn ("Input: [1,2,3,4,5], Expected Output: 9, Success: " ++ show (testA1_Odds [1, 2, 3, 4, 5] 9))
  putStrLn ("Input: [5], Expected Output: 5, Success: " ++ show (testA1_Odds [5] 5))
  putStrLn ("Input: [2,4], Expected Output: 0, Success: " ++ show (testA1_Odds [2, 4] 0))
  putStrLn ("Input: [], Expected Output: 0, Success: " ++ show (testA1_Odds [] 0))
  putStrLn "\nA1 nodes:"
  putStrLn ("Input: tree1, Expected Output: 5, Success: " ++ show (testA1_nodes tree1 5))
  putStrLn ("Input: tree2, Expected Output: 1, Success: " ++ show (testA1_nodes tree2 1))

-- A.2
testA2 :: Ord a => [a] -> [a] -> Bool
testA2 list expected = actual == expected
  where
    actual = merge_sort list

runA2tests :: IO ()
runA2tests = do
  putStrLn "\nA2 merge_sort:"
  putStrLn ("Input: [5,4,3,2,1], Expected Output: [1,2,3,4,5], Success: " ++ show (testA2 [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]))
  putStrLn ("Input: [1,2,3,4,5], Expected Output: [1,2,3,4,5], Success: " ++ show (testA2 [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]))
  putStrLn ("Input: [1,9,2,6,8,3,7,4,5], Expected Output: [1,2,3,4,5,6,7,8,9], Success: " ++ show (testA2 [1, 9, 2, 6, 8, 3, 7, 4, 5] [1, 2, 3, 4, 5, 6, 7, 8, 9]))
  putStrLn ("Input: [5], Expected Output: [5], Success: " ++ show (testA2 [5] [5]))

-- A.3
testA3 :: Int -> [Integer] -> Bool
testA3 tAm expected = actual == expected
  where
    actual = take tAm generiere_fib_strom

testA3_Filter :: Int -> (Integer -> Bool) -> [Integer] -> Bool
testA3_Filter tAm f expected = actual == expected
  where
    actual = take tAm $ filter f generiere_fib_strom

runA3tests :: IO ()
runA3tests = do
  putStrLn "\nA3 generiere_fib_strom:"
  putStrLn ("Take 5, Expected Output: [0,1,1,2,3], Success: " ++ show (testA3 5 [0, 1, 1, 2, 3]))
  putStrLn ("Take 13, Expected Output: [0,1,1,2,3,5,8,13,21,34,55,89,144], Success: " ++ show (testA3 13 [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]))
  putStrLn ("Take 5, Filter > 100, Expected Output: [144,233,377,610,987], Success: " ++ show (testA3_Filter 5 (> 100) [144, 233, 377, 610, 987]))

-- A.4
testA4 :: Double -> Epsilon -> Double -> Bool
testA4 x eps expected = approximiere_exp x eps == expected

runA4tests :: IO ()
runA4tests = do
  putStrLn "\nA4 approximiere_exp:"
  putStrLn ("X = 5, Epsilon = 0.001, Expected Output: 148.4129510721643, Success: " ++ show (testA4 5 0.001 148.4129510721643))
  putStrLn ("X = 0, Epsilon = 0.001, Expected Output: 1.0, Success: " ++ show (testA4 0 0.001 1.0))
  putStrLn ("X = -5, Epsilon = 0.001, Expected Output: 6.863137228200562e-3, Success: " ++ show (testA4 (-5) 0.001 6.863137228200562e-3))

-- A.5
testA5 :: Int -> [String] -> Bool
testA5 tAm expected = actual == expected
  where
    actual = take tAm generiere_woerter

testA5_filter_prim_a :: Int -> [String] -> Bool
testA5_filter_prim_a tAm expected = actual == expected
  where
    actual = take tAm $ filtere_prim_a generiere_woerter

runA5tests :: IO ()
runA5tests = do
  putStrLn "\nA5 generiere_woerter:"
  putStrLn ("Take 5, Expected Output: ['','a','b','c','aa'], Success: " ++ show (testA5 5 ["", "a", "b", "c", "aa"]))
  putStrLn ("Take 15, Expected Output: ['',..,'aab'] Success: " ++ show (testA5 15 ["", "a", "b", "c", "aa", "ab", "ac", "ba", "bb", "bc", "ca", "cb", "cc", "aaa", "aab"]))
  putStrLn "\nA5 filtere_prim_a:"
  putStrLn ("Take 5, Expected Output: ['aa','aaa','aab','aac','aba'], Success: " ++ show (testA5_filter_prim_a 5 ["aa", "aaa", "aab", "aac", "aba"]))
  putStrLn ("Take 15, Expected Output: ['aa',..,'aacb'] Success: " ++ show (testA5_filter_prim_a 15 ["aa", "aaa", "aab", "aac", "aba", "aca", "baa", "caa", "aaab", "aaac", "aaba", "aabb", "aabc", "aaca", "aacb"]))

-- A.6
testA6 :: Int -> [String] -> Bool
testA6 tAm expected = actual == expected
  where
    actual = take tAm $ filtere_palindrome generiere_woerter

testA6_tranf :: Int -> [String] -> Bool
testA6_tranf tAm expected = actual == expected
  where
    actual = take tAm $ transf generiere_woerter

testA6_f_tranf :: Int -> [String] -> Bool
testA6_f_tranf tAm expected = actual == expected
  where
    actual = take tAm $ transf $ filtere_palindrome generiere_woerter

runA6tests :: IO ()
runA6tests = do
  putStrLn "\nA6 filtere_palindrome:"
  putStrLn ("Take 5, Expected Output: ['','a','b','c','aa'], Success: " ++ show (testA6 5 ["", "a", "b", "c", "aa"]))
  putStrLn ("Take 15, Expected Output: ['',..,'cbc'] Success: " ++ show (testA6 15 ["", "a", "b", "c", "aa", "bb", "cc", "aaa", "aba", "aca", "bab", "bbb", "bcb", "cac", "cbc"]))
  putStrLn "\nA6 tranf:"
  putStrLn ("Take 5, Expected Output: ['','A','B','C','AA'], Success: " ++ show (testA6_tranf 5 ["", "A", "B", "C", "AA"]))
  putStrLn ("Take 15, Expected Output: ['',..,'AAB'] Success: " ++ show (testA6_tranf 15 ["", "A", "B", "C", "AA", "AB", "AC", "BA", "BB", "BC", "CA", "CB", "CC", "AAA", "AAB"]))
  putStrLn "\nA6 tranf + filtere_palindrome:"
  putStrLn ("Take 5, Expected Output: ['','A','B','C','AA'], Success: " ++ show (testA6_f_tranf 5 ["", "A", "B", "C", "AA"]))
  putStrLn ("Take 15, Expected Output: ['',..,'CBC'] Success: " ++ show (testA6_f_tranf 15 ["", "A", "B", "C", "AA", "BB", "CC", "AAA", "ABA", "ACA", "BAB", "BBB", "BCB", "CAC", "CBC"]))

-- A.7
testA7 :: Int -> [Char] -> [String] -> Bool
testA7 tAm chars expected = actual == expected
  where
    actual = take tAm $ wort_gen chars

testA7_filter :: Int -> [Char] -> [String] -> Bool
testA7_filter tAm chars expected = actual == expected
  where
    actual = take tAm $ filtere_palindrome $ wort_gen chars

testA7_tranf :: Int -> [Char] -> [String] -> Bool
testA7_tranf tAm chars expected = actual == expected
  where
    actual = take tAm $ transf $ wort_gen chars

testA7_f_tranf :: Int -> [Char] -> [String] -> Bool
testA7_f_tranf tAm chars expected = actual == expected
  where
    actual = take tAm $ transf $ filtere_palindrome $ wort_gen chars

runA7tests :: IO ()
runA7tests = do
  putStrLn "\nA6 wort_gen:"
  putStrLn ("Take 5, Chars: ['X','y','s'], Expected Output: ['X','y','s','XX','Xy'], Success: " ++ show (testA7 5 ['X', 'y', 's'] ["X", "y", "s", "XX", "Xy"]))
  putStrLn ("Take 15, Chars: ['X','y','s'], Expected Output: ['X',..,'XXs'], Success: " ++ show (testA7 15 ['X', 'y', 's'] ["X", "y", "s", "XX", "Xy", "Xs", "yX", "yy", "ys", "sX", "sy", "ss", "XXX", "XXy", "XXs"]))
  putStrLn ("Take 5, Chars: [], Expected Output: [], Success: " ++ show (testA7 5 [] []))
  putStrLn "\nA6 wort_gen + filtere_palindrome:"
  putStrLn ("Take 5, Chars: ['X','y','s'], Expected Output: ['X','y','s','XX','yy'], Success: " ++ show (testA7_filter 5 ['X', 'y', 's'] ["X", "y", "s", "XX", "yy"]))
  putStrLn ("Take 15, Chars: ['X','y','s'], Expected Output: ['X',..,'sss'], Success: " ++ show (testA7_filter 15 ['X', 'y', 's'] ["X", "y", "s", "XX", "yy", "ss", "XXX", "XyX", "XsX", "yXy", "yyy", "ysy", "sXs", "sys", "sss"]))
  putStrLn ("Take 5, Chars: [], Expected Output: [], Success: " ++ show (testA7_filter 5 [] []))
  putStrLn "\nA6 wort_gen + transf:"
  putStrLn ("Take 5, Chars: ['X','y','s'], Expected Output: ['X','Y','S','XX','XY'], Success: " ++ show (testA7_tranf 5 ['X', 'y', 's'] ["X", "Y", "S", "XX", "XY"]))
  putStrLn ("Take 15, Chars: ['X','y','s'], Expected Output: ['X',..,'XXS'], Success: " ++ show (testA7_tranf 15 ['X', 'y', 's'] ["X", "Y", "S", "XX", "XY", "XS", "YX", "YY", "YS", "SX", "SY", "SS", "XXX", "XXY", "XXS"]))
  putStrLn ("Take 5, Chars: [], Expected Output: [], Success: " ++ show (testA7_tranf 5 [] []))
  putStrLn "\nA6 wort_gen + transf + filtere_palindrome:"
  putStrLn ("Take 5, Chars: ['X','y','s'], Expected Output: ['X','Y','S','XX','YY'], Success: " ++ show (testA7_f_tranf 5 ['X', 'y', 's'] ["X", "Y", "S", "XX", "YY"]))
  putStrLn ("Take 15, Chars: ['X','y','s'], Expected Output: ['X',..,'SSS'], Success: " ++ show (testA7_f_tranf 15 ['X', 'y', 's'] ["X", "Y", "S", "XX", "YY", "SS", "XXX", "XYX", "XSX", "YXY", "YYY", "YSY", "SXS", "SYS", "SSS"]))
  putStrLn ("Take 5, Chars: [], Expected Output: [], Success: " ++ show (testA7_f_tranf 5 [] []))