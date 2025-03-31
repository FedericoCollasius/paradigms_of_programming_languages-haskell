module Guia1 where

-- CURRIFICACION Y TIPOS
-- Ejercicio 1
-- i.
max2 :: Float -> Float -> Float
max2 x y 
 | x >= y = x
 | otherwise = y

normaVectorial :: Float -> Float -> Float
normaVectorial x y = sqrt(x**2 + y**2)

subtract1 :: Float -> Float -> Float 
subtract1 = flip (-)

predecesor :: Float -> Float 
predecesor = subtract1 1

evaluarEnCero :: (Float -> Float) -> Float
evaluarEnCero = \f -> f 0 

dosVeces :: (Float -> Float) -> (Float -> Float)
dosVeces = \f -> f . f

--  Map siempre se aplica a listas
--  map :: (a -> b) -> [a] -> [b]
--  flip :: (a -> b -> c) -> b -> a -> c
--  La funcion toma una lista de operaciones binarias aplicadas a float y las flipea
flipAll :: [(Float -> Float -> Float)] -> [(Float -> Float -> Float)]
flipAll = map flip

-- Ohhhh, no aplico flip a una funcion dos veces, sino sí seria (a -> b -> c) -> (a - >b -> c) sino que 
-- le aplico flip a flip
-- Funcion, flip1 :: (a -> b -> c) -> b -> a -> c
-- Argumento, flip2 :: (a' -> b' -> c') -> b' -> a' -> c'
-- flip2 tiene que matchear con el argumento de flip1
-- Es decir,
-- a = (a' -> b' -> c')
-- b = b'
-- c = (a' -> c')
-- Ahora, flip1 devuelve una funcion de tipo b -> a -> c
-- Luego sustituyendo tenemos b' -> (a' -> b' -> c') -> a' -> c'
-- El flip original tomaba una funcion primero y despues un b. 
-- Despues de aplicar flip a si misma toma un b primero despues una funcion
flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip

-- ii.

-- Currying es el proceso de transformar una funcion que toma multiples argumentos en una tupla en una funcion que toma solamente un argumento y devuelve otra funcion que acepta mas arguementos, uno por uno.

-- Queremos las que NO estan currificadas, es decir las que toman argumentos como tuplas
max2Curry :: Float -> Float -> Float
max2Curry x y  
 | x <= y = y
 |  otherwise = x

normaVectorialCurry :: Float -> Float -> Float
normaVectorialCurry x y = sqrt(x**2 + y**2)



-- Ejercicio 2
-- i.
-- El chiste es que le saca los parentesis
curry :: ((a,b) -> c) -> (a -> (b -> c)) 
curry f x y = f (x,y) 

-- ii.
uncurry :: (a -> b -> c) -> (a,b) -> c 
uncurry f (x,y) = f x y

-- iii.
-- Definiendo cosas no asquerosas parece que no



-- Ejercicio 3
-- i. 
sumFoldr :: Num a => [a] -> a
sumFoldr (x:xs) = foldr (+) x xs

elemFoldr :: Eq a => a -> [a] -> Bool
elemFoldr x xs = foldr (\y acc -> x == y || acc) False xs

-- Solucion de Fire
elemFoldr2 :: Eq a => a -> [a] -> Bool
elemFoldr2 x = foldr ((||).(x==)) False

appendFoldr :: [a] -> [a] -> [a]
appendFoldr xs ys = foldr (:) ys xs

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f = foldr (\x xs -> if f x then x:xs else xs) []

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr ((:) . f) []

-- ii. 
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x y -> if f x y then x else y) 

-- iii.
sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldr (\x xs -> x : map(x+) xs) []

-- iv.
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (\x y -> x - y) 0

-- v.
sumaAltInv :: Num a => [a] -> a
sumaAltInv = foldl (flip (-)) 0 



-- Ejercico 5
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                      then [x]
                                      else x : elementosEnPosicionesPares (tail xs)

-- 1. El caso base es un valor fijo: []
-- 2. Pero en el paso recursivo no usa xs sino que tail xs. Ademas usa xs por fuera de la recursion en "null xs"
-- Por lo tanto no es recursion estructural 

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                              then x : entrelazar xs []
                              else x : head ys : entrelazar xs (tail ys)

-- 1. El caso base es un valor fijo: entrelazar [] = id [] = []
-- 2.




--- Ejercicio 6
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z []     = z
recr f z (x:xs) = f x xs (recr f z xs)

-- a. Dados un elemento y una lista devuelve el resultado de eliminar de la lista la primera aparicion del elemento (si está presente)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e [] = []
sacarUna e (x:xs) = if e == x then xs else x : (sacarUna e xs)

sacarUnaRecr :: Eq a => a -> [a] -> [a]
sacarUnaRecr e = recr (\x xs r -> if x == e then xs else x : r) []

-- b. ¿Por qué el esquema de recursión estructural (foldr) no es adecuado para implementar la función sacarUna del punto anterior?

-- Porque foldr ...

-- c. Inserta un elemento en una lista ordenada (de manera creciente), de manera que se preserva el ordenamiento.

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs r -> if x > e then e:x:xs else x:r) []



-- Ejercicio 7
-- i. Una versión de map que toma una función currificada de dos argumentos y una lista de pares de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry.

mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = foldr ((:) . Guia1.uncurry f) [] 

-- ii. Dadas dos listas arma una lista de pares que contiene, en cada posición, el elementocorrespondiente a esa posición en cada una de las listas. Si una de las listas es más larga que la otra,ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en Haskell se llama zip. Pista: aprovechar la currificación y utilizar evaluación parcial.

--armarPares :: [a] -> [b] -> [(a,b)]
--armarPares = id 



-- Clase Practica 25/03/2025

-- Que tipo tiene flip ($) ?
flipm :: a -> (a -> b) -> b
flipm = flip ($)
-- EJ: flipm 0 (+3) = 3

-- Definir funcion equivalente a la siguiente funcion usando map y filter:
listaComp :: (a -> Bool) -> (a -> b) -> [a] -> [b]
listaComp p f xs = [f x | x <- xs, p x]

listComp2 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
listComp2 p f = map f. filter p 
