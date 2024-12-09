module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where

import Test.HUnit
import Data.List(nub)

--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
--E.g., at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
-- E.g., t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.

-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right

        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines


--Ejercicio 1
procVacio :: Procesador a b
procVacio _ = []
procVacio _ = []

procId :: Procesador a a
procId = (:[])

procCola :: Procesador [a] a
procCola [] = []
procCola (x:xs) = xs

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose (Rose _ hijos) = hijos

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT Nil = []
procHijosAT (Tern _ izq centro der) = [izq, centro, der]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie (TrieNodo valor _) = [valor]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo _ subtries) = subtries


--Ejercicio 2
foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b
foldAT cAT cNil at = case at of
                     Nil -> cNil
                     Tern r i c d -> cAT r (rec i) (rec c) (rec d)
    where rec = foldAT cAT cNil

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose cRose (Rose n hijos) = cRose n (map rec hijos)
    where rec = foldRose cRose

foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie f (TrieNodo valor subtries) = f valor [(c, foldTrie f trie) | (c, trie) <- subtries]

--Ejercicio 3
-- Esquema de recursion estructural porque solamente accedo al actual elemento
-- de la lista en cada paso
unoxuno :: Procesador [a] [a]
unoxuno = foldr (\x rec -> [x] : rec) []

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z []     = z
recr f z (x:xs) = f x xs (recr f z xs)

-- Esquema de recursion primitiva porque necesito acceder a la anterior lista
-- para agregar el nuevo elemento prefijado
sufijos :: Procesador [a] [a]
sufijos = recr (\x xs rec -> (x:xs) : rec) [[]]


--Ejercicio 4
-- Veo primero la raiz, despues el subarbol izquierdo, el del medio y el derecho
preorder :: Procesador (AT a) a
preorder = foldAT (\r i c d -> [r] ++ i ++ c ++ d) []

-- Veo primero el subarbol izquierdo, despues el del medio, el derecho y la raiz
postorder :: Procesador (AT a) a
postorder = foldAT (\r i c d -> i ++ c ++ d ++ [r]) []

-- Veo primero el subarbol izquierdo, despues el del medio, la raiz y el derecho
inorder :: Procesador (AT a ) a
inorder = foldAT (\r i c d -> i ++ c ++ [r] ++ d) []


--Ejercicio 5
-- Al principio no veia porque era necesario el concat y no el ++. La idea es que
-- como la cantidad de subarboles no esta acotada, no podemos usar ++ ya que este
-- espera dos listas del mismo tipo. Luego hay que aplanar los resultados de cada subarbol.
-- Para eso usamos concat

-- Como preorder se define como 'primero la raiz, despues los hijos de izq a derecha',
-- la idea es que vamos a poner siempre al elemento actual al final de la lista ya que
-- no estamos mas que recorriendo una lista en orden
preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\n recs -> concat ([n]:recs))

-- Queremos solo las hojas, luego nos quedamos con el nodo n solo cuando recs es vacio
hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\n recs -> if null recs then [n] else concat recs)

-- Similar a hojasRose solamente que cuando la lista de hijos es no vacia, agregamos el nodo actual a
-- todas las listas acumuladas en recs
ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\n recs -> if null recs then [[n]] else map(n:) (concat recs))


--Ejercicio 6
caminos :: Trie a -> [String]
caminos = foldTrie f
  where
    -- hoja sin valor: solo camino vacío
    f Nothing [] = [""]
    -- nodo vacio: camino vacío y concatenación de caminos de hijos
    f Nothing hijos = "" : [c:resto | (c, paths) <- hijos, resto <- paths]
    -- hoja con valor : valor actual
    f (Just _) [] = [""]
    -- nodo con valor: valor actual concatenado con caminos de hijos
    f (Just _) hijos = "" : [c:resto | (c, paths) <- hijos, resto <- paths]


--Ejercicio 7
palabras :: Trie a -> [String]
palabras = nub . foldTrie f
  where
    -- hoja y nothing: no es palabra, no la agrego
    f Nothing [] = []
    -- nodo Nothing : retorna concatenación de caminos de hijos
    f Nothing hijos = concatMap (\(c, paths) -> map (c:) paths) hijos
    -- hoja con valor : su valor
    f (Just _) [] = [""]
    -- nodo con valor : retorna su valor concatenado a caminos de hijos
    f (Just _) hijos = "" : concatMap (\(c, paths) -> map (c:) paths) hijos


--Ejercicio 8
-- 8.a)
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc func proc1 proc2 = \x -> if func x then proc1 x else proc2 x

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) proc1 proc2 = \x -> proc1 x ++ proc2 x


-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) proc1 proc2 = \x -> concat (map proc1 (proc2 x))

--Ejercicio 9
-- Se recomienda poner la demostración en un documento aparte, por claridad y prolijidad, y, preferentemente, en algún formato de Markup o Latex, de forma de que su lectura no sea complicada.


{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]

testsEj1 = test [
    "procVacio con tipo diferente" ~:
        procVacio (undefined :: String) ~?= ([] :: [String]),

    "procId con cadena" ~:
        procId "hola" ~?= ["hola"],

    "procCola con un solo elemento" ~:
        procCola [42] ~?= [],

    "procCola con múltiples elementos" ~:
        procCola ["a", "b", "c"] ~?= ["b", "c"],

    "procHijosRose con un solo hijo" ~:
        let tree = Rose 1 [Rose 2 []]
        in procHijosRose tree ~?= [Rose 2 []],

    "procHijosRose sin hijos" ~:
        let tree = Rose 1 []
        in procHijosRose tree ~?= [],

    "procHijosAT con nodos Nil" ~:
        let tree = Tern 1 Nil Nil Nil
        in procHijosAT tree ~?= [Nil, Nil, Nil],

    "procHijosAT con todos hijos llenos" ~:
        let tree = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
        in procHijosAT tree ~?= [Tern 2 Nil Nil Nil, Tern 3 Nil Nil Nil, Tern 4 Nil Nil Nil],

    "procRaizTrie con valor Just en un nodo hijo" ~:
        let trie = TrieNodo Nothing [ ('a', TrieNodo (Just 'b') []) ]
        in procRaizTrie trie ~?= [Nothing],

    "procSubTries con múltiples subtries" ~:
        let trie = TrieNodo (Just 'r') [
                ('a', TrieNodo (Just 'a') []),
                ('b', TrieNodo (Just 'b') [])]
        in procSubTries trie ~?= [('a', TrieNodo (Just 'a') []), ('b', TrieNodo (Just 'b') [])]
  ]
testsEj2 = test [
    "foldAT Nil" ~:
    foldAT (\_ _ _ _ -> "No es Nil") "es Nil" Nil ~?= "es Nil",

    "foldAT Tern" ~:
    foldAT (\r i c d -> [r] ++ i ++ c ++ d)
           []
           (Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)) ~?=
           [1, 2, 3, 4],

    "foldRose" ~:
        foldRose (\n hijos -> [n] ++ concat hijos) (Rose 1 [Rose 2 [], Rose 3 []]) ~?=
        [1, 2, 3],

   "foldTrie" ~:
   foldTrie (\v subtries -> case v of
                                            Nothing -> "Nothing"
                                            Just x -> "Just " ++ show x ++ " " ++ show subtries)
                          (TrieNodo (Just 1) [('a', TrieNodo Nothing []), ('b', TrieNodo (Just 2) [])]) ~?=
                          "Just 1 [('a',\"Nothing\"),('b',\"Just 2 []\")]"
  ]

testsEj3 = test [
  -- unoxuno
  "unoxuno con listas que tienen listas" ~:
    unoxuno [[3,1],[4],[],[5,7,3]] ~?= [[[3,1]], [[4]], [[]], [[5,7,3]]],
  "unoxuno con lista de Maybe Floats" ~:
    unoxuno [Just 4.1, Nothing, Just 5.2, Nothing] ~?= [[Just 4.1], [Nothing], [Just 5.2], [Nothing]],
  "unoxuno con lista de funciones" ~:
    map (\fs -> map (\f -> f 3) fs) (unoxuno [(*2), (+3), (`div` 2)]) ~?= [[6], [6], [1]],
  --- sufijos
  "sufijos con lista de listas" ~:
    sufijos [[0,1,2], [3], [4,5,6]] ~?= [[[0,1,2], [3], [4,5,6]], [[3], [4,5,6]], [[4,5,6]], []],
  "sufijos con lista de listas vacías" ~:
    sufijos ([[], [], []] :: [[Int]]) ~?= [[[],[],[]], [[],[]], [[]], []],
  "sufijos con palabra" ~:
    sufijos "recursion" ~?= ["recursion","ecursion","cursion","ursion","rsion","sion","ion","on","n",""]
  ]

testsEj4 = test [
  -- vacio
  "preorder vacio" ~:
    preorder Nil ~?= ([] :: [Int]),
  "postorder vacio" ~:
    postorder Nil ~?= ([] :: [Int]),
  "inorder vacio" ~:
    inorder Nil ~?= ([] :: [Int]),
  -- no vacio
  "preorder" ~:
    preorder at ~?= [8,3,1,2,6,4,5,7,10,9],
  "postorder" ~:
    postorder at ~?= [1,2,3,4,5,7,6,9,10,8],
  "inorder" ~:
    inorder at ~?= [1,2,3,4,5,6,7,8,9,10]
  ]
  where
    at = Tern 8
                 (Tern 3
                   (Tern 1 Nil Nil Nil)
                   (Tern 2 Nil Nil Nil)
                   Nil)
                 (Tern 6
                   (Tern 4 Nil Nil Nil)
                   (Tern 5 Nil Nil Nil)
                   (Tern 7 Nil Nil Nil))
                 (Tern 10
                   (Tern 9 Nil Nil Nil)
                   Nil
                   Nil)

testsEj5 = test [
 -- preorderRose
 "preorderRose un solo nodo" ~:
    preorderRose (Rose 1 []) ~?= [1],
 "preorderRose con valores repetidos" ~:
    preorderRose (Rose 1 [Rose 2 [Rose 1 []], Rose 1 [], Rose 2 []]) ~?= [1,2,1,1,2],
 "preorderRose con rosetree grande" ~:
    preorderRose rt ~?= [5,3,8,1,4,7,2,9,6,10],
 -- hojasRose
 "hojasRose con un solo nodo" ~:
    hojasRose (Rose 1 []) ~?= [1],
 "hojasRose con rosetree grande" ~:
    hojasRose rt ~?= [8,1,2,6,10],
 "hojasRose con hojas pares" ~:
    filter even (hojasRose rt) ~?= [8,2,6,10],
 -- ramasRose
 "ramasRose un solo nodo" ~:
    ramasRose (Rose 1 []) ~?= [[1]],
 "ramasRose con rosetree grande" ~:
    ramasRose rt ~?= [[5,3,8],[5,3,1],[5,4,7,2],[5,9,6],[5,9,10]],
 "ramasRose longitud de la rama mas larga" ~:
    maximum (map length (ramasRose rt)) ~?= 4
 ]
 where rt = Rose 5 [Rose 3 [Rose 8 [], Rose 1 []], Rose 4 [Rose 7 [Rose 2 []]], Rose 9 [Rose 6 [], Rose 10 []]]

testsEj6 = test [
  "caminos empty" ~:
    caminos (TrieNodo Nothing []) ~?= [""],
  "caminos single node" ~:
    caminos (TrieNodo (Just True) []) ~?= [""],
  "caminos example" ~:
    caminos (TrieNodo Nothing [
      ('a', TrieNodo (Just True) []),
      ('b', TrieNodo Nothing [
        ('a', TrieNodo (Just True) [
          ('d', TrieNodo Nothing [])
        ])
      ]),
      ('c', TrieNodo (Just True) [])
    ]) ~?= ["", "a", "b", "ba", "bad", "c"],
  "caminos - trie con caminos largos" ~:
    caminos (TrieNodo Nothing [
      ('h', TrieNodo Nothing [
        ('o', TrieNodo Nothing [
          ('l', TrieNodo Nothing [
            ('a', TrieNodo (Just True) [])
          ])
        ])
      ]),
      ('c', TrieNodo Nothing [
        ('a', TrieNodo Nothing [
          ('s', TrieNodo Nothing [
            ('a', TrieNodo (Just True) [])
          ])
        ])
      ])
    ]) ~?= ["", "h", "ho", "hol", "hola", "c", "ca", "cas", "casa"],
  "caminos - trie con nodos internos con valor" ~:
    caminos (TrieNodo (Just True) [
      ('a', TrieNodo (Just True) [
        ('b', TrieNodo (Just True) [
          ('c', TrieNodo (Just True) [])
        ])
      ])
    ]) ~?= ["", "a", "ab", "abc"],
  "caminos - trie con ramas vacías" ~:
    caminos (TrieNodo Nothing [
      ('a', TrieNodo Nothing []),
      ('b', TrieNodo (Just True) []),
      ('c', TrieNodo Nothing [
        ('d', TrieNodo Nothing [])
      ])
    ]) ~?= ["", "a", "b", "c", "cd"]
  ]

testsEj7 = test [
  "palabras empty" ~:
    palabras (TrieNodo Nothing []) ~?= [],
  "palabras single node" ~:
    palabras (TrieNodo (Just True) []) ~?= [""],
  "palabras example" ~:
    palabras (TrieNodo (Just True) [
      ('a', TrieNodo (Just True) []),
      ('b', TrieNodo Nothing [
        ('a', TrieNodo (Just True) [
          ('d', TrieNodo Nothing [])
        ])
      ]),
      ('c', TrieNodo (Just True) [])
    ]) ~?= ["", "a", "ba", "c"],
  "palabras - trie con prefijos compartidos" ~: 
    palabras (TrieNodo Nothing [
      ('p', TrieNodo Nothing [
        ('r', TrieNodo Nothing [
          ('e', TrieNodo (Just True) [
            ('s', TrieNodo Nothing [
              ('a', TrieNodo (Just True) []),
              ('o', TrieNodo (Just True) [])
            ])
          ])
        ])
      ])
    ]) ~?= ["pre", "presa", "preso"],
  "palabras - trie con palabras anidadas" ~:
    palabras (TrieNodo Nothing [
      ('a', TrieNodo (Just True) [
        ('n', TrieNodo (Just True) [
          ('t', TrieNodo (Just True) [
            ('e', TrieNodo (Just True) [])
          ])
        ])
      ])
    ]) ~?= ["a", "an", "ant", "ante"],
  "palabras - trie con ramas sin palabras" ~:
    palabras (TrieNodo Nothing [
      ('a', TrieNodo Nothing []),
      ('b', TrieNodo (Just True) []),
      ('c', TrieNodo Nothing [
        ('d', TrieNodo Nothing [
          ('e', TrieNodo (Just True) [])
        ])
      ])
    ]) ~?= ["b", "cde"]
  ]

testsEj8a = test [
    "ifProc true case" ~:
        (ifProc (== 1) procId procVacio) 1 ~?= [1],

    "ifProc false case" ~:
        (ifProc (== 1) procId procVacio) 2 ~?= [],

    "ifProc with posorder and preorder case true" ~:
        let tree = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
        in (ifProc (\(Tern r _ _ _) -> r == 1) postorder preorder) (tree :: AT Int) ~?= [2,3,4,1],

    "ifProc with posorder and preorder case false" ~:
        let tree = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
        in (ifProc (\(Tern r _ _ _) -> r == 2) postorder preorder) tree ~?= ([1,2,3,4] :: [Int])
    ]

-- Test cases for ++!
testsEj8b = test [
    "++! with procId and procVacio" ~:
        ((procId ++! procVacio) 1) ~?= [1],

    "++! with procVacio and procId" ~:
        ((procVacio ++! procId) 1) ~?= [1],

    "++! with procId and procId" ~:
        ((procId ++! procId) 1) ~?= [1, 1],

    "++! with procHijosRose and procHijosRose" ~:
        let tree = Rose 1 [Rose 2 [], Rose 3 []]
        in (procHijosRose ++! procHijosRose) tree ~?= [Rose 2 [], Rose 3 [], Rose 2 [], Rose 3 []],

    "++! with procHijosAT and procHijosAT" ~:
        let tree = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
        in (procHijosAT ++! procHijosAT) tree ~?= [Tern 2 Nil Nil Nil, Tern 3 Nil Nil Nil, Tern 4 Nil Nil Nil, Tern 2 Nil Nil Nil, Tern 3 Nil Nil Nil, Tern 4 Nil Nil Nil],

    "++! with procHijosAT and procHijosRose" ~:
        let tree = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
        in (procHijosAT ++! procId) tree ~?= [Tern 2 Nil Nil Nil, Tern 3 Nil Nil Nil, Tern 4 Nil Nil Nil, Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)]
    ]

-- Test cases for .!
testsEj8c = test [
    ".! with procId and procId" ~:
        ((procId .! procId) 1) ~?= [1],

    ".! with procId and procVacio" ~:
        ((procId .! procVacio) 1 :: [Int]) ~?= [],

    ".! with procVacio and procId" ~:
        ((procVacio .! procId) 1 :: [Int]) ~?= [],

    "procId .! procHijosRose" ~:
        let tree = Rose 1 [Rose 2 []]
        in (procId .! procHijosRose) tree ~?= [Rose 2 []],

    "procId .! procHijosAT" ~:
        let tree = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
        in (procId .! procHijosAT) tree ~?= [Tern 2 Nil Nil Nil, Tern 3 Nil Nil Nil, Tern 4 Nil Nil Nil],

    "procId .! procHijosAT" ~:
        let tree = Tern 1 (Tern 2 Nil Nil (Tern 3 Nil Nil Nil)) (Tern 4 Nil Nil Nil) (Tern 5 Nil Nil Nil)
        in (procHijosAT .! procHijosAT) tree ~?=[Nil,  Nil,Tern 3 Nil  Nil  Nil,  Nil,  Nil,  Nil,  Nil,  Nil,  Nil]
    ]
