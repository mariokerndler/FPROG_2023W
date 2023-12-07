> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Use camelCase" #-}
> {-# LANGUAGE DatatypeContexts #-}

> module UE6LHS where

> import Data.Maybe(fromMaybe)
> import Data.List (intercalate)

------------------------------------------------------------------------------------
-- A1
------------------------------------------------------------------------------------

> data Argumentbereich = A1 | A2 | A3 | A4 | A5 | A6 deriving (Show, Eq, Enum)

> type Bildbereich = Integer

> type Fkt = (Argumentbereich -> Bildbereich)

> type Gph = [(Argumentbereich, Bildbereich)]

> f_zu_g :: Fkt -> Gph
> f_zu_g f = [(x, f x) | x <- [A1 .. A6]]

> g_zu_f :: Gph -> Fkt
> g_zu_f g x =
>   case lookup x g of
>     Just y -> y
>     Nothing -> error "Ungültiges Argument"

------------------------------------------------------------------------------------
-- A2
------------------------------------------------------------------------------------

> newtype (Eq a, Enum a, Eq b) => Funktion a b = F (a -> b)
> newtype (Eq a, Enum a, Eq b) => Graph a b = G [(a,b)]
> type Liste a b = [(a,b)]

> fkt_zu_gph :: (Eq a, Enum a, Eq b) => Funktion a b -> Graph a b
> fkt_zu_gph (F f) = G [(x, f x) | x <- [toEnum 0 ..]]

> gph_zu_fkt :: (Eq a, Enum a, Eq b) => Graph a b -> Funktion a b
> gph_zu_fkt (G g) = F (\x -> fromMaybe (error "Ungültiges Argument") (lookup x g))

> grh_zu_lst :: (Eq a, Enum a, Eq b) => Graph a b -> Liste a b
> grh_zu_lst (G g) = g

> lst_zu_gph :: (Eq a, Enum a, Eq b) => Liste a b -> Graph a b
> lst_zu_gph = G

> ftk_zu_lst :: (Eq a, Enum a, Eq b) => Funktion a b -> Liste a b
> ftk_zu_lst (F f) = [(x, f x) | x <- [toEnum 0 ..]]

> lst_zu_fkt :: (Eq a, Enum a, Eq b) => Liste a b -> Funktion a b
> lst_zu_fkt l = F (\x -> fromMaybe (error "Ungültiges Argument") (lookup x l))

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

> type Wahrheitswert = Bool

> data LogAusdruck 
>   = LK Wahrheitswert -- LK fuer logische Konstante
>   | Und LogAusdruck LogAusdruck -- Logische Konjunktion
>   | Oder LogAusdruck LogAusdruck -- Logische Disjunktion
>   | Nicht LogAusdruck -- Logische Negation
>   | Gleich ArithAusdruck ArithAusdruck -- Wertegleichheit zweier arithm. Ausdruecke
>   | Kleiner ArithAusdruck ArithAusdruck -- Wert des ersten Ausdruck kleiner Wert des zweiten

-- Hilfsfunktion zur Auswertung der Zahlendaten

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

> aaw :: ArithAusdruck -> Zustand -> Wert
> aaw = aw

> ala :: LogAusdruck -> Zustand -> Wahrheitswert
> ala (LK w) _ = w
> ala (Und l1 l2) z = ala l1 z && ala l2 z
> ala (Oder l1 l2) z = ala l1 z || ala l2 z
> ala (Nicht l) z = not (ala l z)
> ala (Gleich a1 a2) z = aaw a1 z == aaw a2 z
> ala (Kleiner a1 a2) z = aaw a1 z < aaw a2 z

------------------------------------------------------------------------------------
-- A4
------------------------------------------------------------------------------------

> data Anweisung
>   = Zw
>       { ls :: Variable, -- ls fuer ‘linke Seite der Zuweisung’
>         rs :: ArithAusdruck -- rs fuer ‘rechte Seite der Zuweisung’
>       } -- Zw f. Zuweisung ‘Variable := Ausdruck’
>   | Skip -- Skip fuer die leere Anweisung ‘noop’
>   | If LogAusdruck Anweisung Anweisung -- Fallunterscheidungsanweisung
  
> type Programm = [Anweisung]

> int :: Programm -> Zustand -> Zustand
> int [] z = z
> int (Zw var ausdruck : rest) z = int rest (updateZustand var (aw ausdruck z) z)
> int (Skip : rest) z = int rest z
> int (If bedingung thenZweig elseZweig : rest) z
>   | ala bedingung z = int (thenZweig : rest) z
>   | otherwise = int (elseZweig : rest) z

-- Hilfsfunktion zur Aktualisierung des Zustands

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
  
> showZustand :: Zustand -> IO ()
> showZustand z = print $ "{ " ++ showVariables ++ " }"
>     where
>       variables = [A, B, C, R, S, T, X, Y, Z]
>       showVariables = intercalate ", " [show v ++ " -> " ++ show (z v) | v <- variables]
