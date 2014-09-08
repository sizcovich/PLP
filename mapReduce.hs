module MapReduce where

import Data.Ord
import Data.List

-- ---------------------------------Sección 1---------Diccionario ---------------------------
type Dict k v = [(k,v)]

-- Ejercicio 1
belongs :: Eq k => k -> Dict k v -> Bool
belongs m = foldr (\x y -> (m == (fst x)) || y) False
-- Dado un elemento m y un diccionario, se recorre a este último de derecha a izquierda y se compara
-- a m con cada clave, uniendo cada comparación por un "OR". En el caso en el que coincida con algún
-- elemento, devuelve True. Caso contrario, False.

(?) :: Eq k => Dict k v -> k -> Bool
(?) xs m = belongs m xs
-- Evalúa la función belongs con un diccionario y una clave.

-- Ejercicio 2
get :: Eq k => k -> Dict k v -> v
get m xs = foldr 	(\x y -> if (m == (fst x)) then (snd x) else y) 
					(snd (head xs))
					xs
-- Dado un elemento m y un diccionario, lo recorre de derecha a izquierda comparando cada clave con
-- m. Una vez que encuentra la clave m, devuelve el significado de la misma. Dado que una vez que se
-- encuentra la clave correspondiente se devuelve directamente el significado sin que se lo combine
-- con resultados parciales, no afecta en el resultado el valor de inicio, que en nuestro caso es
-- (snd (head xs)).

(!) :: Eq k => Dict k v -> k -> v
(!) xs m = get m xs
-- Evalúa la función get con un diccionario y una clave.

-- Ejercicio 3
insertWith :: Eq k => (v -> v -> v) -> k -> v -> Dict k v -> Dict k v
insertWith f m l xs = if ((?) xs m)
                      then (foldr (\x rec -> 	if (m == fst x)
                      							then ((fst x, (f (snd x) l)):rec)
                      							else (x:rec)) [] xs) 
                      else (xs++[(m,l)])
-- Dada una función f, una clave m, un significado l y un diccionario, si la clave m pertenece a l
-- ((?) xs m), entonces se la compara con cada elemento del diccionario y una vez que se la
-- encuentra la agrega al diccionario combinando su significado con l a través de la función f. En
-- el caso en el que no esté, se concatena la clave con el significado l al diccionario.

-- Ejercicio 4
groupByKey :: Eq k => [(k,v)] -> Dict k [v]
groupByKey = foldl	(\y x -> (insertWith (++) (fst x) ([snd x]) y)) []
-- Para cada elemento del arreglo ingresado, se llama a la función insertWith con la función
-- concatenar, el primer elemento de la tupla, un arreglo conteniendo el segundo elemento y el resto
-- de los elementos del arreglo. De este modo, se concatenan los significados de cada elemento si es
-- que se encuentran repetidos y sino, son agregados normalmente a la salida de la función.

-- Ejercicio 5
unionWith :: Eq k => (v -> v -> v) -> Dict k v -> Dict k v -> Dict k v
unionWith f xs ys = foldl (\y x -> (insertWith f (fst x) (snd x) y))
							[]
							(xs++ys)
-- Dada la concatenación de dos diccionarios (xs++ys), toma cada elemento del primero y aplica insertWith de
-- una función f con la clave y el significado de cada uno de ellos. De este modo, se elimina la
-- posibilidad de tener claves repetidas en dicha concatenación.

-- ------------------------------Sección 2--------------MapReduce---------------------------

type Mapper a k v = a -> [(k,v)]
type Reducer k v b = (k, [v]) -> [b]


-- Ejercicio 6
distributionProcess :: Int -> [a] -> [[a]]
distributionProcess i = foldr (\m ns -> tail(ns) ++ [m:head(ns)])
								(replicate i [])

-- Ejercicio 7
mapperProcess :: Eq k => Mapper a k v -> [a] -> [(k,[v])]
mapperProcess xs ys = groupByKey (foldr (\x y -> (xs x)++y) [] ys)
-- Dado un elemento de tipo Mapper a k v y un arreglo, le aplica Mapper a cada elemento obteniendo
-- un resultado de tipo [(k,v)] y lo concatena con la salida generada (que comienza con un arreglo
-- vacío). Por último, concatena los significados por su clave a través de groupByKey.

pruebaMapper :: (String,String) -> [(String,Char)]
pruebaMapper (x,y) = [(y,'I')]
-- Función de prueba para el Mapper. Permite crear el ejemplo del enunciado. Se puede probar el
-- ejemplo a través de mapperProcess (pruebaMapper)
-- [("Pablo","Berlin"),("Gabriela","Amsterdam"),("Taihu","Amsterdam")]

-- Ejercicio 8
combinerProcess :: (Eq k, Ord k) => [[(k, [v])]] -> [(k,[v])]
combinerProcess xss = order (foldr(\x y -> unionWith (++) x y) [] xss)

order::(Eq a, Ord a)=>[(a,b)]->[(a,b)]
order = foldr	insertarOrdenado []

insertarOrdenado::(Eq k, Ord k) => (k, v) -> [(k, v)] -> [(k, v)]
insertarOrdenado x xs = [less | less <- xs , (fst less) <= (fst x)] ++ [x] ++ [greater | greater <- xs , (fst greater) > (fst x)]

-- Ejercicio 9
reducerProcess :: Reducer k v b -> [(k, [v])] -> [b]
reducerProcess red ls = concat (foldr	(\x rec -> (red x) : rec)
											[[]]
											ls)

reducerExample :: (k, v) -> [k]
reducerExample par = [fst par]

pruebaReducer :: (k, [v]) -> [(k, Int)]
pruebaReducer par = [(fst par, length (snd par))]

-- Ejercicio 10 
mapReduce :: (Eq k, Ord k) => Mapper a k v -> Reducer k v b -> [a] -> [b]
mapReduce fMap fRed ls = reducerProcess fRed combinedList

							where combinedList = combinerProcess mappedProcess
								-- mappedProcess :: [[(k,[v])]]		(Input)
								-- combinedProcess :: [(k,[v])]		(Output)

								where mappedProcess =  (foldr	(\x rec -> (mapperProcess fMap x) : rec)
																[[]]
																distributionList)

									-- Output :: [[(k,[v])]]

									where distributionList = distributionProcess 100 ls

-- Ejercicio 11
visitasPorMonumento :: Ord a => [a] -> Dict a Int
visitasPorMonumento = mapReduce divisionMapper monumentReducer

divisionMapper :: a -> Dict a Int
divisionMapper x = [(x,1)]

-- visitasPorMonumento aplica mapReduce a la función divisionMapper y monumentReducer.
-- divisionMapper se encarga de, dado un elemento x, crear un arreglo de tuplas donde el primer
-- elemento de la tupla agregada es x y el segundo el valor 1. Por otro lado, monumentReducer crea,
-- dada una tupla de tipo (a, [b]), un arreglo de tuplas de tipo (a, b) donde el segundo elemento
-- consiste en la sumatoria del arreglo de la tupla ingresada.

-- Ejercicio 12
monumentosTop :: Ord a => [a] -> [a]
monumentosTop xs = mapReduce topMapper topReducer (visitasPorMonumento xs)

topMapper :: (a, Int) -> [(Int, a)]
topMapper (x,y) = [(-y,x)]

topReducer :: (Int, [a]) -> [a]
topReducer par = snd par

-- Ejercicio 13 
monumentosPorPais :: [(Structure, Dict String String)] -> [(String, Int)]
monumentosPorPais = mapReduce monumentMapper monumentReducer


monumentMapper :: (Structure, Dict String String) -> [(String, Int)]
monumentMapper entry =	if ( (fst entry) == Monument )
						then [(get "country" (snd entry), 1)]
						else []


monumentReducer :: (a, [Int]) -> [(a, Int)]
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
