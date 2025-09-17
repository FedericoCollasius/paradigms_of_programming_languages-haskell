{-
Definir las siguientes funciones:
- curry :: ((a,b) -> c) -> (a -> b -> c)
que devuelve la versi ́on currificada de una funci ́on no
currificada 

uncurry :: (a -> b -> c) -> ((a,b) -> c)
que devuelve la versi ́on no currificada de una funcion
currificada.

-}

curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x -> \y -> f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \(x, y) -> f x y

{-
Sea la funcion:
prod :: Int -> Int -> Int
prod x y = x * y

Definimos doble x = prod 2 x

1 ¿Cual es el tipo de doble?
doble :: Int -> Int 

2 ¿Que pasa si cambiamos la definici ́on por doble = prod 2?
Nada, sigue siendo una funcion que toma un Int y devuelve un Int

3 ¿Que significa (+) 1?
El equivalente a f(x) = x + 1

4 Definir las siguientes funciones de forma similar a (+)1:
triple :: Float -> Float
triple = (*) 3
esMayorDeEdad :: Int -> Bool
esMayorDeEdad = > 18
-}


-- Implementar y dar los tipos de las siguientes funciones:
-- a. (.) que compone dos funciones.
-- (.) :: (b -> c) -> (a -> b) -> a -> c 
-- (.) g f x = g Prelude.$ f x

-- b. flip que invierte los argumentos de una funcion. 
-- flip :: (a -> b -> c) -> b -> a -> c 
-- flip f y x = f x y

-- c. aplica una funcion a un argumento
 -- ($) :: (a -> b) -> a -> b
-- ($) f x = f x

-- d. const que, dado un valor, retorna una funcíon constante que
const :: a -> b -> a
const x y = x

-- ¿Que hace flip ($) 0?
-- Invierte el orden de como $ recibe los operandos:
-- ($) :: (a -> b) -> a -> b pasa a ($) :: a -> (a -> b) -> b

-- ¿Y (==0) . (flip mod 2)?
-- Por partes, si mod esta definido como 
-- mod :: Int -> Int -> Int
-- mod a b = a % b
-- Luego flip mod 2:
-- flip mod 2 :: Int -> Int       -- aplicación parcial
-- flip mod 2 x = mod x 2         -- equivalente a: x `mod` 2
-- Basicamente armamos una funcion para ver si un numero es modulo 2
-- Componiendolo con (==0), que va a tomar el resultado de (b % 2) y
-- ver si es igual a 0 luego hicimos una funcion para chequear si un
-- numero es par

-- fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)
-- [0, 1] : zipWith (+) [0, 1] [1]

deLongitudN :: Int -> [[a]] -> [[a]]
deLongitudN n = filter(\xs -> length xs == n)

soloPuntosFijosEnN :: Int -> [Int -> Int] -> [Int -> Int]
soloPuntosFijosEnN n = filter(\f -> f n == n)

reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado xs = reverse (map reverse xs)

paresCuadrados :: [Int] -> [Int]
paresCuadrados = map (\x -> if even x then x^2 else x)

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = 
  foldr(\x acc -> case f x of 
    Just e -> e : acc
    Nothing -> acc
   ) 
  [] 
-- Es total

listComp :: (a -> Bool) -> (a -> b) -> [a] -> [b]
listComp p f = map f . filter p
