module PPON where

import Documento
import GHC.Settings.Utils (getRawBooleanSetting)
import Distribution.Simple.Command (OptDescr(BoolOpt))

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico ppon = case ppon of 
                    TextoPP _  -> True
                    IntPP _    -> True
                    ObjetoPP _ -> False

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple ppon = case ppon of
                          TextoPP _        -> True
                          IntPP _          -> True
                          ObjetoPP []      -> False
                          ObjetoPP (x:xs)  -> foldr(\x acc -> acc || pponAtomico (snd x)) False (x:xs)


intercalar :: Doc -> [Doc] -> Doc
intercalar = error "PENDIENTE: Ejercicio 7"

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar = error "PENDIENTE: Ejercicio 8"

pponADoc :: PPON -> Doc
pponADoc = error "PENDIENTE: Ejercicio 9"
