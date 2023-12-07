> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Use camelCase" #-}
> module UE5LHS where

------------------------------------------------------------------------------------
-- A1
------------------------------------------------------------------------------------

> map2 :: (a -> b) -> (a -> b) -> [a] -> [b]
> map2 _ _ [] = []
> map2 f g (x : xs) = f x : map2 g f xs

------------------------------------------------------------------------------------
-- A2
------------------------------------------------------------------------------------

> map_n :: [a -> b] -> [a] -> [b]
> map_n _ [] = []
> map_n [] _ = []
> map_n fs xs = zipWith ($) (cycle fs) xs

------------------------------------------------------------------------------------
-- A3
------------------------------------------------------------------------------------

> data Zahl
>   = N -- N fuer ‘Null’
>   | P Zahl -- P fuer ‘plus eins’
>   | M Zahl -- M fuer ‘minus eins’
>   deriving (Show)

> data Variable = A | B | C | R | S | T | X | Y | Z deriving (Eq, Show)

> data ArithAusdruck
>   = K Zahl -- K fuer Konstante
>   | V Variable -- V fuer Variable
>   | Ps ArithAusdruck ArithAusdruck -- Ps fuer plus
>   | Ml ArithAusdruck ArithAusdruck -- Ml fuer mal
>   | Ms ArithAusdruck ArithAusdruck -- Ms fuer minus
>   | Abs ArithAusdruck -- Abs fuer Absolutbetrag
>   deriving (Show)

> type Zustand = Variable -> Zahl -- Total definierte Abbildungen

> type Wert = Integer

Hilfsfunktion zur Auswertung der Zahlendaten

> evalZahl :: Zahl -> Wert
> evalZahl N = 0
> evalZahl (P n) = evalZahl n + 1
> evalZahl (M n) = evalZahl n - 1

> aw :: ArithAusdruck -> Zustand -> Wert
> aw (K zahl) _ = evalZahl zahl
> aw (V var) z = evalZahl (z var)
> aw (Ps e1 e2) z = aw e1 z + aw e2 z
> aw (Ml e1 e2) z = aw e1 z * aw e2 z
> aw (Ms e1 e2) z = aw e1 z - aw e2 z
> aw (Abs e) z = abs (aw e z)

------------------------------------------------------------------------------------
-- A4
------------------------------------------------------------------------------------

> data Anweisung
>   = Zw
>       { ls :: Variable, -- ls fuer ‘linke Seite der Zuweisung’
>         rs :: ArithAusdruck -- rs fuer ‘rechte Seite der Zuweisung’
>       } -- Zw f. Zuweisung ‘Variable := Ausdruck’
>   | Skip -- Skip fuer die leere Anweisung ‘noop’

> type Programm = [Anweisung]

> int :: Programm -> Zustand -> Zustand
> int [] z = z
> int (Zw var ausdruck : rest) z = int rest (updateZustand var (aw ausdruck z) z)
> int (Skip : rest) z = int rest z

Hilfsfunktion zur Aktualisierung des Zustands

> updateZustand :: Variable -> Integer -> Zustand -> Zustand
> updateZustand var wert z v = if v == var then toEnum $ fromIntegral wert else z v

> instance Enum Zahl where
>   toEnum n
>     | n == 0 = N
>     | n > 0 = P (toEnum (n - 1))
>     | otherwise = M (toEnum (n + 1))

>   fromEnum N = 0
>   fromEnum (P x) = 1 + fromEnum x
>   fromEnum (M x) = fromEnum x - 1