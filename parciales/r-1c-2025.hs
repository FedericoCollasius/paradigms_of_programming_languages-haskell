import Data.List (nub, intersect)

data LineaProd =  Materiales [String]
                | Agregar String LineaProd
                | Unir LineaProd LineaProd

foldLineaProd :: ([String] -> a) -> (String -> a -> a) -> (a -> a -> a) -> LineaProd -> a
foldLineaProd cMateriales cAgregar cUnir lp = case lp of
    Materiales xs -> cMateriales xs
    Agregar s lp' -> cAgregar s (rec lp')
    Unir lp1 lp2 -> cUnir (rec lp1) (rec lp2)
  where rec = foldLineaProd cMateriales cAgregar cUnir

recLineaProd :: ([String] -> a) ->
                (String -> LineaProd -> a -> a) ->
                (LineaProd -> LineaProd -> a -> a -> a) ->
                LineaProd -> a
recLineaProd cMateriales cAgregar cUnir lp = case lp of
    Materiales xs -> cMateriales xs
    Agregar s lp' -> cAgregar s lp' (rec lp')
    Unir lp1 lp2 -> cUnir lp1 lp2 (rec lp1) (rec lp2)
  where rec = recLineaProd cMateriales cAgregar cUnir

materialesUsados :: LineaProd -> [String]
materialesUsados = nub . foldLineaProd id (:) (++)

subLineasDisjuntas :: LineaProd -> Bool
subLineasDisjuntas = recLineaProd
                      (const True)
                      (\_ _ rec -> rec)
                      (\lp1 lp2 rec1 rec2 ->  null (materialesUsados lp1 `intersect` materialesUsados lp2) && rec1 && rec2)

mismaEstructura :: LineaProd -> LineaProd -> Bool
mismaEstructura = foldLineaProd 
                    (\l1 -> \lp2 -> case lp2 of
                                    Materiales _ -> True
                                    _            -> False
                    )
                    (\m rec -> \lp2 -> case lp2 of
                                      Agregar _ lp' -> rec lp'
                                      _ -> False
                    )
                    (\rec1 rec2 -> \lp2 -> case lp2 of
                                      Unir lp0 lp1 -> rec1 lp0 && rec2 lp1
                                      _ -> False
                    )

