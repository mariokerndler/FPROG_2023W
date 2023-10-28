module UE2 where

type Nat0 = Integer
data Quer = EchtQuer | UnechtQuer | NichtQuer deriving (Eq, Show)

------------------------------------------------------------------------------------
-- A1   
------------------------------------------------------------------------------------
quer :: Nat0 -> Quer
quer n
    | n < 2 = UnechtQuer
    | isPrime n = UnechtQuer
    | otherwise = if quersumme n == sum (map quersumme (primeFactor n))
                    then EchtQuer
                    else NichtQuer

-- Beschreibung
-- Funktion 'quer' check zuerst ob eine Zahl Unecht Quer ist mit hilfe der 'isPrime' 
-- Funktion wird gecheckt ob die Zahl eine Primzahl ist. Ist dies nicht der Fall
-- Wird gecheckt ob die Quersumme gleich der Summe alle Quersummen der Primfaktoren von 
-- der Zahl ist. Ist dies der Fall dann ist die Zahl echt Quer ansonsten nicht Quer.

------------------------------------------------------------------------------------
-- A2
------------------------------------------------------------------------------------
verflechte3 :: [Int] -> [Int] -> [Int] -> [Int]
verflechte3 [] [] [] = []
verflechte3 l1 l2 l3 = headOf l1 ++ headOf l2 ++ headOf l3 ++ verflechte3 (tailOf l1) (tailOf l2) (tailOf l3)

-- Beschreibung
-- Funktion 'verflechte3' checkt zuerst den Basiscase, dass alle Listen leer sind,
-- ist dies nicht der Fall so nehmen wir von jeder Liste das erste Element und haengen 
-- sie an das Ergebnis der Rekursion an. Die Rekursion wird mit dem Rest der Listen 
-- aufgerufen.

------------------------------------------------------------------------------------
-- A3
------------------------------------------------------------------------------------
verflechte3' :: ([Int], [Int], [Int]) -> [Int]
verflechte3' ([], [], []) = []
verflechte3' (l1, l2, l3) = headOf l1 ++ headOf l2 ++ headOf l3 ++ verflechte3' (tailOf l1, tailOf l2, tailOf l3)

-- Beschreibung
-- Funktionsweise von 'verflechte3'' ist gleich der von Funktion 'verflechte3',
-- nur das hier ein trippel uebergeben wird.

------------------------------------------------------------------------------------
-- A4
------------------------------------------------------------------------------------
verzopfe1 :: [[Int]] -> [Int]
verzopfe1 lists
    | all null lists = []
    | otherwise = concatMap headOf lists ++ verzopfe1 (map tailOf lists)
    
-- Beschreibung
-- Funktion 'verzopfe1' check zuerst den Basiscase, dass alle Listen Leer sind,
-- ist dies nicht der fall mappen wir die Funktion 'headOf' an alle Listen und 
-- fuegen alle elemente anschliessen zusammen (concatMap). Das Ergebnis von
-- 'concatMap' wird anschliesen mit dem Ergebnis der Rekursion zusammengefuegt.
-- In der Rekursion mappen wird die Funktion 'tailOf' an alle Listen. 
    
------------------------------------------------------------------------------------
-- A5
------------------------------------------------------------------------------------
verzopfe2 :: [[Int]] -> [Int]
verzopfe2 [] = []
verzopfe2 lists
    | length lists == 2 = verzopfe1 lists
    | length lists == 1 = head lists    
verzopfe2 (x:y:xs) = verzopfe2 (t : xs)
    where 
        t = verzopfe1 [x, y]
        
-- Beschreibung
-- Funktion 'verzopfe2' nimmt sich das Patternmatching von Listen zur hilfe.
-- Ist die Liste leer, ist das Ergebnis leer. Besteht die Liste aus genau 2 
-- Elementen werden diese mit Hilfe von der Funktion 'verzopfe1' verzopft.
-- Besteht die Liste nur aus einem Element, wird dies als Ergebnis zurueckgegeben.
-- Ansonsten nehmen wir immer die ersten beiden Elemente, verzopfen diese und 
-- verzopfen diese weiter mit dem Rest.
        
------------------------------------------------------------------------------------
-- A6
------------------------------------------------------------------------------------
curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f l1 l2 l3 = f (l1, l2, l3)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (l1, l2, l3) = f l1 l2 l3

-- Beschreibung
-- Funktion 'curry3' nimmt eine Funktion f welche ein Triple (a,b,c) auf d abbildet
-- und 3 weiter Parameter welche gleich denjenigen aus dem Triple sein muessen und
-- ruft Funktion f mit den Parametern auf.

-- Funktion 'uncurry3' nimmt Funktion f welche (a -> b -> c -> d) und ruft damit
-- das Triple (a,b,c) auf. 

------------------------------------------------------------------------------------
-- Hilfsfunktionen   
------------------------------------------------------------------------------------

-- Hilfsfunktion zur berechnung der Quersumme einer Zahl
quersumme :: Nat0 -> Nat0
quersumme 0 = 0
quersumme n = n `mod` 10 + quersumme (n `div` 10)

-- Hilfsfunktion zur ueberpruefung, ob eine Zahl eine Primzahl ist
isPrime :: Nat0 -> Bool
isPrime n
    | n <= 1 = False
    | n <= 3 = True
    | otherwise = all (\x -> n `mod` x /= 0) [2..isqrt n]
    where isqrt = floor . sqrt . fromIntegral
    
-- Hilfsfunktion zur berechnung der Primfaktoren einer Zahl
primeFactor :: Nat0 -> [Nat0]
primeFactor n = factor n 2
    where factor 1 _ = []
          factor n f
            | n `mod` f == 0 = f : factor (n `div` f) f
            | otherwise = factor n (f + 1)
            
-- Selbstdefinierte 'head' funktion, die auch auf leere Listen funktioniert
headOf :: [Int] -> [Int]
headOf [] = []
headOf (x:xs) = [x]

-- Selbstdefinierte 'tail' funktion, die auch auf leere Listen funktioniert
tailOf :: [Int] -> [Int]
tailOf [] = []
tailOf (x:xs) = xs