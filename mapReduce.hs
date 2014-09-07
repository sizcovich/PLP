module MapReduce where

import Data.Ord
import Data.List

-- ---------------------------------Sección 1---------Diccionario ---------------------------
type Dict k v = [(k,v)]

-- Ejercicio 1
belongs :: Eq k => k -> Dict k v -> Bool
belongs m = foldr 	(\x y -> (m == (fst x)) || y)
						False


(?) :: Eq k => Dict k v -> k -> Bool
(?) xs m = belongs m xs
--Main> [("calle",[3]),("city",[2,1])] ? "city" 
--True

-- Ejercicio 2
get :: Eq k => k -> Dict k v -> v
get m xs = foldr 	(\x y -> if (m == (fst x)) then (snd x) else y) 
					(snd (head xs))
					xs

(!) :: Eq k => Dict k v -> k -> v
(!) xs m = get m xs
--Main> [("calle",[3]),("city",[2,1])] ! "city" 
--[2,1]

-- Ejercicio 3
insertWith :: Eq k => (v -> v -> v) -> k -> v -> Dict k v -> Dict k v
insertWith f m l xs = if ((?) xs m)
                      then (foldr 	(\x rec -> 	if (m == fst x)
                      							then ((fst x, (f (snd x) l)):rec)
                      							else (x:rec))
                      				[]
                      				xs ) 
                      else (xs++[(m,l)])
--Main> insertWith (++) 2 ['p'] (insertWith (++) 1 ['a','b'] (insertWith (++) 1 ['l'] []))
--[(1,"lab"),(2,"p")]

-- Ejercicio 4
groupByKey :: Eq k => [(k,v)] -> Dict k [v]
groupByKey = foldl	(\y x -> (insertWith (++) (fst x) ([snd x]) y))
					[]
--Main> groupByKey [("calle","Jean␣Jaures"),("ciudad","Brujas"), ("ciudad","Kyoto"),("calle","7")]
--[("calle",["Jean␣Jaures","7"]),("ciudad",["Brujas","Kyoto"])]

-- Ejercicio 5
unionWith :: Eq k => (v -> v -> v) -> Dict k v -> Dict k v -> Dict k v
unionWith f xs ys = foldl 	(\y x -> (insertWith f (fst x) (snd x) y))
							[]
							(xs++ys)
--Main> unionWith (++) [("calle",[3]),("city",[2,1])] [("calle", [4]), ("altura", [1,3,2])]
--[("calle",[3,4]),("city",[2,1]),("altura",[1,3,2])]


-- ------------------------------Sección 2--------------MapReduce---------------------------

type Mapper a k v = a -> [(k,v)]
type Reducer k v b = (k, [v]) -> [b]


-- Ejercicio 6
distributionProcess :: Int -> [a] -> [[a]]
distributionProcess i = foldr 	(\m ns -> tail(ns) ++ [m:head(ns)])
								(replicate i [])


-- Ejercicio 7
mapperProcess :: Eq k => Mapper a k v -> [a] -> [(k,[v])]
mapperProcess xs ys = groupByKey (foldr 	(\x y -> (xs x)++y)
											[]
											ys)
--mapperProcess (pruebaMapper) [("Pablo","Berlin"),("Gabriela","Amsterdam"),("Taihu","Amsterdam")]

pruebaMapper :: (String,String) -> [(String,Char)]
pruebaMapper (x,y) = [(y,'I')]

-- Ejercicio 8
combinerProcess :: (Eq k, Ord k) => [[(k, [v])]] -> [(k,[v])]
combinerProcess xss = order (foldr	(\x y -> unionWith (++) x y)
									[]
									xss)

order::(Eq a, Ord a)=>[(a,b)]->[(a,b)]
order = foldr	insertarOrdenado
					[]

insertarOrdenado::(Eq k, Ord k) => (k, v) -> [(k, v)] -> [(k, v)]
insertarOrdenado x xs = [less | less <- xs , (fst less) <= (fst x)] ++ [x] ++ [greater | greater <- xs , (fst greater) > (fst x)]

-- Ejercicio 9
reducerProcess :: Reducer k v b -> [(k, [v])] -> [b]
reducerProcess red ls = 	concat (foldr	(\x rec -> (red x) : rec)
											[[]]
											ls)



reducerExample :: (k, v) -> [k]
reducerExample par = [fst par]

pruebaReducer :: (k, [v]) -> [(k, Int)]
pruebaReducer par = [(fst par, length (snd par))]

-- Ejercicio 10 
mapReduce :: (Eq k, Ord k) => Mapper a k v -> Reducer k v b -> [a] -> [b]
mapReduce fMap fRed ls = 	reducerProcess fRed combinedList

							where combinedList = combinerProcess mappedProcess
								-- mappedProcess :: [[(k,[v])]]		(Input)
								-- combinedProcess :: [(k,[v])]		(Output)

								where mappedProcess =  (foldr	(\x rec -> (mapperProcess fMap x) : rec)
																[[]]
																distributionList)

									-- Output :: [[(k,[v])]]

									where distributionList = distributionProcess 100 ls

-- Ejercicio 11
visitasPorMonumento :: [String] -> Dict String Int
visitasPorMonumento = mapReduce divisionMapper monumentReducer

divisionMapper :: String -> Dict String Int
divisionMapper x = [(x,1)]

-- Ejercicio 12
monumentosTop :: [String] -> [String]
monumentosTop xs = mapReduce topMapper topReducer (visitasPorMonumento xs)

topMapper :: (String, Int) -> [(Int, String)]
topMapper (x,y) = [(-y,x)]

topReducer :: (Int, [String]) -> [String]
topReducer par = snd par

-- Ejercicio 13 
monumentosPorPais :: [(Structure, Dict String String)] -> [(String, Int)]
monumentosPorPais = mapReduce monumentMapper monumentReducer


monumentMapper :: (Structure, Dict String String) -> [(String, Int)]
monumentMapper entry =	if ( (fst entry) == Monument )
						then [(get "country" (snd entry), 1)]
						else []


monumentReducer :: (String, [Int]) -> [(String, Int)]
monumentReducer entry = [(fst entry, sum (snd entry))]


-- ------------------------ Ejemplo de datos del ejercicio 13 ----------------------
data Structure = Street | City | Monument deriving (Show, Eq)

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