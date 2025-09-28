last :: [a] -> a
last = head . reverse

maximum :: Ord a => [a] -> a
maximum = foldr1 max

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\ x best -> if f x best then x else best)

elem :: Eq a => a -> [a] -> Bool
elem e = foldr (\x acc -> e == x || acc) False 

takeRecursionExplicita :: [a] -> Int -> [a]
takeRecursionExplicita [] _ = []
takeRecursionExplicita (x:xs) n = if n == 0 then [] else x : (takeRecursionExplicita xs (n-1))

-- foldr :: (a -> b -> b) -> b -> [a] -> [b]
-- foldr cVacio cBin l = case l of
--                          []      -> cVacio
--                          (x: xs) -> cBin x (rec xs) 
--      where rec = foldr cVacio cBin


-- f :: (a -> (Int -> [a]) -> (Int -> [a]))
takeFoldr :: [a] -> Int -> [a]
takeFoldr = foldr
        (\x rec n -> if n == 0 then [] else x : (rec (n-1)))
        (const [])

sacarUnaExplicita :: Eq a => a -> [a] -> [a]
sacarUnaExplicita _ [] = [] 
sacarUnaExplicita e (x: xs) = if x == e then xs else sacarUnaExplicita e xs

data Arbol a = Hoja a | Bin (Arbol a) a (Arbol a)

foldAB f g (Hoja n) = f n
foldAB f g (Bin t1 n t2) = g (foldAB f g t1) n (foldAB f g t2)

ramas :: Arbol a -> [[a]]
ramas = foldAB (\h -> [[h]]) (\i r d -> (map (r:) i) ++ (map (r:) d))
