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


