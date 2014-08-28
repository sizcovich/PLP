module MapReduce where

import Data.Ord
import Data.List

-- ---------------------------------Sección 1---------Diccionario ---------------------------
type Dict k v = [(k,v)]

-- Ejercicio 1
belongs :: Eq k => k -> Dict k v -> Bool
belongs m xs = foldr (\x y -> (m == (fst x)) || y) False xs


(?) :: Eq k => Dict k v -> k -> Bool
(?) m xs = belongs xs m
--Main> [("calle",[3]),("city",[2,1])] ? "city" 
--True

-- Ejercicio 2
get :: Eq k => k -> Dict k v -> v
get m xs = foldr (\x y -> if (m == (fst x)) then (snd x) else y) (snd (head xs)) xs

(!) :: Eq k => Dict k v -> k -> v
(!) m xs = get xs m
--Main> [("calle",[3]),("city",[2,1])] ! "city" 
--[2,1]

-- Ejercicio 3
insertWith :: Eq k => (v -> v -> v) -> k -> v -> Dict k v -> Dict k v
insertWith f m l xs = if ((?) xs m) 
                      then (foldr (\x rec -> if (m == fst x) then ((fst x, (f (snd x) l)):rec) else (x:rec)) [] xs) 
                      else (xs++[(m,l)])
--Main> insertWith (++) 2 ['p'] (insertWith (++) 1 ['a','b'] (insertWith (++) 1 ['l'] []))
--[(1,"lab"),(2,"p")]

-- Ejercicio 4
groupByKey :: Eq k => [(k,v)] -> Dict k [v]
--quiero que ponga todas las claves en listas y si dos claves tienen la misma clave que las concatene
-- tomo una tupla, si ya puse la clave en el dict agrego la clave, sino lo pongo con [clave]
groupByKey xs = foldr (\x y -> (insertWith (++) (fst x) ([snd x]) xs)) [] xs
--*Main> groupByKey [("calle","Jean␣Jaures"),("ciudad","Brujas"), ("ciudad","Kyoto"),("calle","7")]
--[("calle",["Jean␣Jaures","7"]),("ciudad",["Brujas","Kyoto"])]

-- Ejercicio 5
unionWith :: Eq k => (v -> v -> v) -> Dict k v -> Dict k v -> Dict k v
unionWith = undefined
--Main> unionWith (++) [("calle",[3]),("city",[2,1])] [("calle", [4]), ("altura", [1,3,2])]
--[("calle",[3,4]),("city",[2,1]),("altura",[1,3,2])]


-- ------------------------------Sección 2--------------MapReduce---------------------------

type Mapper a k v = a -> [(k,v)]
type Reducer k v b = (k, [v]) -> [b]

-- Ejercicio 6
distributionProcess :: Int -> [a] -> [[a]]
distributionProcess = undefined

-- Ejercicio 7
mapperProcess :: Eq k => Mapper a k v -> [a] -> [(k,[v])]
mapperProcess = undefined

-- Ejercicio 8
combinerProcess :: (Eq k, Ord k) => [[(k, [v])]] -> [(k,[v])]
combinerProcess = undefined

-- Ejercicio 9
reducerProcess :: Reducer k v b -> [(k, [v])] -> [b]
reducerProcess = undefined

-- Ejercicio 10
mapReduce :: (Eq k, Ord k) => Mapper a k v -> Reducer k v b -> [a] -> [b]
mapReduce = undefined

-- Ejercicio 11
visitasPorMonumento :: [String] -> Dict String Int
visitasPorMonumento = undefined

-- Ejercicio 12
monumentosTop :: [String] -> [String]
monumentosTop = undefined

-- Ejercicio 13 
monumentosPorPais :: [(Structure, Dict String String)] -> [(String, Int)]
monumentosPorPais = undefined


-- ------------------------ Ejemplo de datos del ejercicio 13 ----------------------
data Structure = Street | City | Monument deriving Show

items :: [(Structure, Dict String String)]
items = [
    (Monument, [
      ("name","Obelisco"),
      ("latlong","-36.6033,-57.3817"),
      ("country", "Argentina")]),
    (Street, [
      ("name","Int. Güiraldes"),
      ("latlong","-34.5454,-58.4386"),
      ("country", "Argentina")]),
    (Monument, [
      ("name", "San Martín"),
      ("country", "Argentina"),
      ("latlong", "-34.6033,-58.3817")]),
    (City, [
      ("name", "Paris"),
      ("country", "Francia"),
      ("latlong", "-24.6033,-18.3817")]),
    (Monument, [
      ("name", "Bagdad Bridge"),
      ("country", "Irak"),
      ("new_field", "new"),
      ("latlong", "-11.6033,-12.3817")])
    ]


------------------------------------------------
------------------------------------------------