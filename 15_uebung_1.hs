module UE1 where

import Data.Char (isDigit)
import GHC.Show ( intToDigit )

------------------------------------------------------------------------------------
-- A1   
------------------------------------------------------------------------------------
h :: Integer -> Integer
h z = product [sum [i^j | j <- [0..i]] | i <- [0..abs z]]

-- Beschreibung
-- Funktion 'h' nimmt den Betrag von 'z', um das Produkt der inneren Summen zu berechnen.

------------------------------------------------------------------------------------
-- A2   
------------------------------------------------------------------------------------
ae :: String -> String -> Int
ae s t
    | length s /= length t = -1
    | otherwise            = length [i | i <- [0..length s - 1], s !! i == t !! i] 
    
-- Beschreibung
-- Funktion 'ae' überprüft zuerst, ob die beiden Eingabezeichenreihen 's' und 't' 
-- die gleiche Länge haben. Wenn sie unterschiedliche Längen haben, wird -1 zurückgegeben.
-- Andernfalls wird eine List von Positionen erstellt, an denen die Zeichen in 's' und 't'
-- übereinstimmen, und die Länge dieser Liste wird zurückgegeben.

------------------------------------------------------------------------------------
-- A3  
------------------------------------------------------------------------------------
ozfw :: String -> (String, Int)
ozfw s = let
    filteredDigits = filter (\c -> isDigit c && c /= '8' && c /= '9') s
    octalValue     = octalToDecimal filteredDigits
    in (filteredDigits, octalValue)
    
-- Beschreibung
-- In der Funktion 'ozfw' wird zuerst 'filteredDigits' erstellt, das alle Ziffern in der Eingabe 
-- enthält, außer den Ziffern 8 und 9. Schließlich wird octalValue mit hilfe der Funktion 
-- 'octalToDecimal' berechnet, 
-- Funktion 'octalToDecimal' konvertiert einen String zu einem Int.
-- Die 'foldl' Funktion akkumuliert das Ergebnis indem es die akkumulierte Zahl mit 8 multipliziert (Oktal Konversion) 
-- und dann wird noch der dezimale Wert des momentan Characters addiert.

-- Wenn die Oktalziffernfolge leer ist, wird das Paar ("", 0) zurückgegeben. 
-- Dies erfüllt die Anforderung, weil es die leere Oktalziffernfolge klar repräsentiert. 
-- Der Wert 0 im Dezimalsystem eine klare und gebräuchliche Darstellung für "keine Ziffern".

octalToDecimal :: String -> Int
octalToDecimal = foldl (\acc c -> acc * 8 + octal c) 0
    where 
        octal c = fromEnum c - fromEnum '0'

------------------------------------------------------------------------------------
-- A4   
------------------------------------------------------------------------------------
verflechte :: [Int] -> [Int] -> [Int]
verflechte [] l' = l'
verflechte l []  = l
verflechte (x:xs) (y:ys) = x : y : verflechte xs ys

-- Beschreibung
-- Funktion 'verflechte' funktioniert mit hilfe von Pattern-Matching:
-- 1. Wenn die erste Liste l leer ist, wird die zweite Liste l' als Ergebnis zurückgegeben.
-- 2. Wenn die zweite Liste l' leer ist, wird die erste Liste l als Ergebnis zurückgegeben.
-- 3. Im allgemeinen Fall wird das erste Element aus l mit dem ersten Element aus l' kombiniert, 
--    gefolgt von der Rekursion auf den Rest der beiden Listen.