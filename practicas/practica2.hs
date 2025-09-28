last :: [a] -> a
last = head . reverse

maximum :: Ord a => [a] -> a
maximum = foldr1 max

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\ x best -> if f x best then x else best)

elem :: Eq a => a -> [a] -> Bool
elem e = foldr (\x acc -> e == x || acc) False 

takeRecursionExplicita :: Int -> [a] -> [a]
takeRecursionExplicita _ [] = []
takeRecursionExplicta n (x:xs) = if n == 0 then [] else x : takeRecursionExplicita (n-1) xs

-- foldr :: (a -> b -> b) -> [a] -> [b]
-- f :: (a -> (Int -> [a]) -> (Int -> [a]))

takeFoldr :: [a] -> Int -> [a]
takeFoldr = foldr (\x rec -> \n -> if n == 0 then [] else x : rec(n-1)) (const [])

sacarUnaExplicita :: Eq a => a -> [a] -> [a]
sacarUnaExplicita e [] = [] 
sacarUnaExplicita e (x: xs) = if x == e then xs else sacarUnaExplicita e xs

sacarUnaFoldr :: Eq a => a -> [a] -> [a]
sacarUnaFoldr e = foldr (\x rec -> if x == e then rec else) (const [])