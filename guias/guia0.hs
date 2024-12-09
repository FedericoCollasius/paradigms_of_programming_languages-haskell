module Guia0 where

--Ejercicio 1
-- null :: Foldable t => t a -> Bool
-- Evalua si una estructura esta vacia.

-- head :: HasCallStack => [a] -> a
-- Extrae el primer elemento de una lista.

-- tail :: HasCallStack => [a] -> [a]
-- La lista sin el primer elemento.

-- init :: HasCallStack => [a] -> [a]
-- La lista sin el ultimo elemento.

-- last :: HasCallStack => [a] -> a
-- Extrae el ultimo elemento de una lista.

-- take :: Int -> [a] -> [a]
-- Crea una lista, Int es la cantidad de elementos que tiene que tomar de [a] para armar la lista.

-- drop :: Int -> [a] -> [a]
-- Tira de la lista los primeros elementos definidos por Int.

-- (++) :: [a] -> [a] -> [a]
-- Concatena dos listas.

-- concat :: [[a]] -> [a]
-- Toma una lista de listas y las concatena.

--  reverse :: [a] -> [a]
-- Da vuelta orden de una lista.

-- elem :: Eq a => a -> t a -> Bool
-- Aparece el elemento en la estructura?


-- Ejercicio 2
-- a
valorAbsoluto :: Float -> Float
valorAbsoluto n
  | n >= 0 = n
  | otherwise = -n

-- b
bisiesto :: Int -> Bool
bisiesto n = (mod n 4 == 0 && mod n 100 /= 0) || mod n 400 == 0

-- c
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1) 

--d
-- Se puede mejorar la performance usando que arranque a chequar desde (sqrt n)
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = cantDivisoresPrimosDesde n n 

cantDivisoresPrimosDesde :: Int -> Int -> Int
cantDivisoresPrimosDesde n 1 = 0
cantDivisoresPrimosDesde n k 
 | esPrimo(k) && n `mod` k == 0 = 1 + cantDivisoresPrimosDesde n (k-1)
 | otherwise = cantDivisoresPrimosDesde n (k-1)

esPrimo :: Int -> Bool
esPrimo n = (cantDivisoresDesde n n) == 2

cantDivisoresDesde :: Int -> Int -> Int
cantDivisoresDesde n 1 = 1
cantDivisoresDesde n k
 | n `mod` k == 0 = 1 + cantDivisoresDesde n (k-1)
 | otherwise = cantDivisoresDesde n (k-1)



-- Ejercicio 3
-- a
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso n = Just(1/n)

-- b
aEntero :: Either Int Bool -> Int
aEntero (Left e) = e 
aEntero (Right e) = if e then 1 else 0 


-- Ejercicio 4
-- a
limpiar :: String -> String -> String
limpiar [] ys = ys
limpiar (x:xs) ys = limpiar xs (extraerTodasLasApariciones x ys)

extraerTodasLasApariciones :: Char -> String -> String
extraerTodasLasApariciones x [] = []
extraerTodasLasApariciones x (y:ys) = if x == y then extraerTodasLasApariciones x ys else y : extraerTodasLasApariciones x ys

-- b
difPromedio :: [Float] -> [Float]
difPromedio l = map (subtract (promedio l)) l

promedio :: [Float] -> Float
promedio [] = 0
promedio n = sum n / fromIntegral (length n)

-- c
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:ls) = x == y && todosIguales (y:ls) 


-- Ejercicio 5
data AB a = Nil | Bin (AB a) a (AB a)
-- Faltaria una funcion para mostrar el arbol 
-- a
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

-- b
negacionAB :: AB Bool -> AB Bool
nagacionAB Nil = Nil
negacionAB (Bin i r d) = Bin (negacionAB i) (not r) (negacionAB d)

-- c
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i r d) = r * (productoAB i) * (productoAB d)
